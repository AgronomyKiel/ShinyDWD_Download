# Library of functions for the handling of weather data
# directly downloaded from the DWD ftp server
# or from the local copy of the DWD ftp server
# or from files in the local RData format
# additionally it contains functions for generating radiation data from sunshine hours
# and for the calculation of potential evapotranspiration

# Libraries ---------------------------------------------------------------

library(stringi)
library(curl)
library(RCurl)
library(scales)
library(stringi)
library(sf)
library(lubridate)
library(fst)
library(data.table)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)
library(here)
library(zoo)


# Directories -------------------------------------------------------------

## Local path ####
#localpath <- "./"
#localpath <- here("/")
localpath <- paste0(here(),"/")

## DWD repositories for synoptic weather data ######## 
DWD_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
DWD_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/"

## DWD repositories for additional rainfall data ########
DWDRain_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/historical/"
DWDRain_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/recent/"


## Local directories for DWD data ####

### Local folders for copies of DWD data ####
LocalCopy_DWD_ftp_historical <- paste0(localpath, "LocalCopyDWD/kl/historical/")
LocalCopy_DWD_ftp_recent <- paste0(localpath,"LocalCopyDWD/kl/recent/")

### Local folders for copies of additional DWD rainfall data ####
LocalCopy_DWD_Rain_ftp_historical <- paste0(localpath,"LocalCopyDWD/more_precip/historical/")
LocalCopy_DWD_Rain_ftp_recent <- paste0(localpath, "LocalCopyDWD/more_precip/recent/")

### local DWD Rdata paths ####
Local_DWD <- paste0(localpath,"LocalCopyDWD/")
Local_R_DWD <- paste0(localpath,"LocalCopyDWD/Rdata/")
Local_DWD_historical <- paste0(localpath,"historical/")
Local_DWD_recent <- paste0(localpath,"recent/")

Local_Rain_DWD_historical <- paste0(localpath,"historical/")
Local_Rain_DWD_recent <- paste0(localpath,"recent/")

### local RData file names ####
HistoricalDWD <- "historicalDWDweather.RData"
HistoricalDWDRain <- "historicalDWDRain.RData"

### outputpath ####
datapath <- paste0(localpath,"data/")
figurepath <- paste0(localpath,"figures/")


# Constants ---------------------------------------------------------------

names_DWD <- c("QN_4", # QUALITAETS_NIVEAU
               "TMK",  # LUFTTEMPERATUR
               "PM",   # LUFTDRUCK_STATIONSHOEHE
               "UPM",  # REL_FEUCHTE
               "FM",   # WINDGESCHWINDIGKEIT
               "TXK",  # LUFTTEMPERATUR_MAXIMUM
               "TNK",  # LUFTTEMPERATUR_MINIMUM
               "RSK",  # NIEDERSCHLAGSHOEHE
               "SDK"   # SONNENSCHEINDAUER
               )

longNames_DWD <- c( "QUALITAETS_NIVEAU",
                    "LUFTTEMPERATUR",
                    "LUFTDRUCK_STATIONSHOEHE",
                    "REL_FEUCHTE",
                    "WINDGESCHWINDIGKEIT",
                    "LUFTTEMPERATUR_MAXIMUM",
                    "LUFTTEMPERATUR_MINIMUM",
                    "NIEDERSCHLAGSHOEHE",
                    "SONNENSCHEINDAUER"
)

units_DWD <- c("code","Tagesmittel der Temperatur °C", "Tagesmittel des Luftdrucks hPa",
               "Tagesmittel der Relativen Feuchte %", "Tagesmittel Windgeschwindigkeit m/s",
               "Tagesmaximum der Lufttemperaturin 2m Höhe °C", "Tagesminimum der Lufttemperaturin 2m Höhe °C",
               "tägliche Niederschlagshöhe mm", "tägliche Sonnenscheindauer h")

df_names_DWD <- data.frame(names_DWD, longNames_DWD, units_DWD)
df_names_DWD <- df_names_DWD %>% mutate(ylables = paste0(longNames_DWD, " [", units_DWD, "]") )



# Names and units for Hume data
namesHUME <- c("Time", "TMPM", "Rain", "LF", "VP", "Sat_def", "Rad_Int", "GlobRad", "Wind", "TMPMN", "TMPMX")
unitsHUME <- c("[day]", "[°C]", "[mm/d]", "[%]", "[mbar]", "[mbar]", "[W/m²]", "[MJ/m²/d]", "[m/s]", "[°C]", "[°C]")
longNamesHUME <- c("Time", "Mittel Lufttemperatur", "Niederschlagshöhe", "Luftfeuchtigkeit",
                   "Dampfdruck", "Sättigungsdampfdruckdefizit", "Globalstrahlung",
                   "Globalstrahlung", "Windgeschwindigkeit", "Minimum Lufttemperatur",
                   "Maximum Lufttemperatur")

df_names_HUME <- data.frame(names = namesHUME, longNames = longNamesHUME, units=unitsHUME)
df_names_HUME <- df_names_HUME %>% mutate(ylabels = paste0(longNamesHUME, " ",unitsHUME) )

# idvars and measvars for the weather data
idvars <- c("Date","Stationsname","geoBreite", "geoLaenge", "Stationshoehe", "Stations_id")

measvars <- c("LUFTTEMPERATUR",
              "LUFTTEMPERATUR_MAXIMUM",
              "LUFTTEMPERATUR_MINIMUM",
              "NIEDERSCHLAGSHOEHE",
              "REL_FEUCHTE",
              "WINDGESCHWINDIGKEIT",
              "EstGlobRad",
              "VP",
              "Sat_def",
              "Rad_Int")

CalcVars <- c("CumRain", "CumRad", "TempSum", "PT", "cPT", "Rn", "ra", "rc", "pETP", "cumETP", "climWbal", "cumWbal")

UnitsCalcVars <- c("[mm]", "[MJ/m²]", "[°Cd]", "[MJ/(°Cd)]", "[mm]", "[MJ/m²]", "[s/m]", "[s/m]", "[mm]", "[mm]", "[mm]", "[mm]")

LongNamesCalcVars <- c("Jahressumme Niederschlag",
                       "Jahressumme Globalstrahlung",
                       "Temperatursumme",
                       "Photothermalwert",
                       "Jahressumme Photothermalwert",
                       "Nettostrahlung", 
                       "Aerodynamischer Widerstand",
                       "Canopy Widerstand",
                       "Potentielle Evapotranspiration",
                       "Jahressumme potentielle Evapotranspiration",
                       "Klimatische Wasserbilanz",
                       "Jahressumme klimatische Wasserbilanz")

df_names_CalcVars <- data.frame(names = CalcVars, longNames = LongNamesCalcVars, units=UnitsCalcVars)
df_names_CalcVars <- df_names_CalcVars %>% mutate(ylabels = paste0(longNames, " ",units) )

df_names <- rbind( df_names_HUME, df_names_CalcVars)

# Functions ---------------------------------------------------------------


# Utility-Functions ---------------------------------------------------------------


#' Title: ReplaceProblemChars
#'
#' @param InputString A string with characters to be replaced
#'
#' @return A string with replaced characters
#' @export
#'
#' @examples
ReplaceProblemChars <- function(InputString=NULL) {
  InputString <- trimws(InputString)
  InputString <- gsub(" / ",replacement = "_", x = InputString)
  InputString <- gsub(" (",replacement = "_", x = InputString, fixed = T)
  InputString <- gsub(")",replacement = "", x = InputString, fixed = T)
  InputString <- gsub(" ",replacement = "_", x = InputString, fixed = T)
  InputString <- gsub(".",replacement = "", x = InputString, fixed = T)
  return(InputString)
}


# Graphic routines ----------------------------------------------------------------

setplotbackground <-  function (plot, BaseSize=18){
  #  plot <- plot +  scale_colour_brewer(palette="Set1")
  plot <- plot + theme_bw(base_size = BaseSize)
  plot <- plot + theme(plot.background = element_rect(fill = NULL,colour = NA))
  plot <- plot + theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))
  plot <- plot + theme(axis.ticks.length = unit(.15, "cm"))
  plot <- plot + theme(axis.ticks = element_line(colour = 'black', size = 1, linetype = 'solid'))
  plot <- plot + theme(axis.text=element_text(colour="black"))
  plot <- plot + theme(panel.border = element_rect(size=1.5, fill=NA, color="black"))
  plot <- plot + theme(panel.background = element_rect(fill = NULL,colour = NA))
#  plot <- plot + theme(plot.background = element_rect(fill = "transparent"))
  plot <- plot + theme(legend.background = element_blank())
}

set_theme <- function(p, fontsize){
#  p <- p + theme_bw(base_family = FontFamilie)
  p <- p + theme(text = element_text(size=fontsize))
  p <- p + theme(panel.border = element_blank())
  p <- p + theme(axis.ticks = element_line(size = 1.2, linetype = "solid", color="black"))
  p <- p + theme(axis.ticks.length=unit(0.2, "cm"))
  p <- p + theme(plot.background = element_rect(fill = NULL,colour = NA))
  p <- p + theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))
  p <- p + theme(panel.border = element_rect(size=1.2, fill=NA))
  p <- p + theme(panel.background = element_blank())
  p <- p + theme(plot.background = element_rect(fill = "transparent", colour = NA))
  p <- p + theme(plot.title =  element_text( face="bold", colour = "black"))
  p <- p + theme(plot.subtitle = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.text = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.title = element_text( face="bold", colour = "black"))
  p <- p + theme(axis.ticks = element_line(size = 1.2, linetype = "solid", color="black"))
  p <- p + theme(axis.ticks.length=unit(0.2, "cm"))
  p <- p + theme(strip.text = element_text(face="bold"))
  p <- p + theme(strip.background =element_blank())
  p <- p + theme(panel.grid.major = element_line(size = 1, linetype = "solid", color="grey"))
  p <- p + theme(panel.grid.minor = element_line(size = 0.5, linetype = "solid", color="grey"))
  p <- p + theme(plot.background = element_rect(fill = NULL,colour = NA))
  p <- p + theme(axis.line = element_line(size = 1, linetype = "solid", color="black"))

}


#' makeplot
#'
#' @param dfweather data frame with weather data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param xlabel The x label string
#' @param SelYear The year to be highlighted
#' @param IsLegend 
#'
#' @return A ggplot object
#' @export
#'
#' @examples
makeplot <- function(dfweather, parameter, BaseSize=18, ylabel="", xlabel="Datum", SelYear=0, IsLegend=TRUE){
  
  y_label <- ifelse(ylabel=="", parameter, ylabel)
  
  tmp <- df_names %>% filter(names==parameter) %>% dplyr::select(ylabels)
  if (nrow(tmp)>0) {
    y_label <- tmp$ylabels
  }
  
  
  selcols <- c("Date", "DOY","Year", parameter)
  stopifnot(parameter %in% names(dfweather))
  dfweather <- dfweather[,selcols]
  dfweather <- as.data.frame(dfweather)
  dfweather$param <- dfweather[,parameter]
  dfweather[,parameter] <- NULL
  df_LT <- dfweather %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.75), na.rm=T), q = c(0.25, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")
  dfExtreme <- dfweather %>% group_by(DOY) %>%
    summarise(median=median(param), max = max(param), min=min(param))
  
  df_stat <- left_join(x = df_LT, y = dfExtreme, by="DOY")
  dfExtreme <- dfExtreme %>% dplyr::select(-median) %>% pivot_longer(cols = c("max", "min"), names_to = "stat", values_to = "value")
  df_l <- df_stat %>% pivot_longer(cols = c("median", "max", "min", "q0.25","q0.75" ), names_to = "stat", values_to = "value")
  df_l <- df_l %>% mutate(lty=ifelse(stat%in% c("min","max"), 2, 1))


  SelYear <- as.numeric(SelYear)
  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
  SelYear <- as.numeric(SelYear)
  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY
  df_l$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_l$DOY
  dfExtreme$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+dfExtreme$DOY
  
  dfsmooth <- df_l %>% 
    group_by(stat) %>% 
    arrange(stat, DOY) %>% 
    nest() %>%
    mutate(pred = purrr::map(data, function(x)stats::loess(value~DOY, span= 0.5, data = x) %>%
                               stats::predict(data.frame(DOY = seq(min(x$DOY), max(x$DOY), 1))))) %>%
    unnest(cols = c(data, pred))
  
  df_ribbon <- dfsmooth %>% filter(stat %in% c("q0.25", "q0.75")) %>% dplyr::select(-value) %>% 
    pivot_wider(names_from = stat, values_from = pred)

  
  Myltys <- c("dotted", "solid", "dotted", "solid", "solid", "solid")
  Mycolors <- c("darkgreen", "darkblue", "darkgreen", "darkgreen", "darkgreen", "darkgreen")
  
 # SelYearValues <- dfweather[dfweather$Year==SelYear, c( "Date","DOY", "param")]
  SelYearValues <-  dfweather %>% filter(Year==SelYear) %>% dplyr::select(Date, DOY, param)
  SelYearValues <- as.data.frame(SelYearValues)
  p <- ggplot()
  p <- p + geom_ribbon(data = df_ribbon, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="green", alpha=0.3)
  p <- p + geom_line(data=dfsmooth, aes(x=Date, y=pred, color=stat, lty=stat), size=0.5)
  p <- p + geom_line(data=dfExtreme, aes(x=Date, y=value, color=stat) , size=0.8, lty="dotted")
#  p <- p + geom_smooth(data=dfExtreme_l, aes(x=Date, y=value, lty = stat), color="red", size=1.2, se = F, 
#                       method = "loess", span = 0.5)
  p <- p + geom_line(data=SelYearValues, aes(x=Date, y=param), color="red", size=1)
  p <- p + scale_x_date(labels = date_format("%b"), date_breaks = "1 month")
  p <- p + ylab(y_label) + xlab(xlabel)
#  p <- p + scale_color_identity(guide = "legend")
  p <- p + scale_linetype_manual(values = Myltys)
  p <- p + scale_color_manual(values = Mycolors)
  p <- p + theme_bw(base_size = BaseSize)
  p
  return(p)
}



#' makeplot2 a function to plot scenarios
#'
#' @param dfhist data frame with historic data
#' @param dfscen data frame with scen data
#' @param parameter Column to be plotted
#' @param BaseSize Base char size for the plot
#' @param ylabel The y label string
#' @param SelYear The year to be highlighted
#'
#' @return A ggplot object
#' @export
#'
#' @examples
makeplot2 <- function(dfhist, dfscen, parameter, BaseSize=18, ylabel="", SelYear=0){
  # select columns
  selcols <- c("Date", "DOY","Year", parameter)
  # check if parameter is in the data frame
  stopifnot(parameter %in% names(dfhist))
  # select columns
  dfhist <- dfhist[,selcols]
  # convert to data frame
  dfhist <- as.data.frame(dfhist)
  # create a new column with the parameter values
  dfhist$param <- dfhist[,parameter]
  dfhist[,parameter] <- NULL
  # calculate statistics for the historic data
  df_LT <- dfhist %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.5, 0.75), na.rm=T), q = c(0.25, 0.5, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")
  
  # create a new data frame with extreme values
  dfExtreme <- dfhist %>% group_by(DOY) %>%
    summarise(max = max(param), min=min(param), mean=mean(param, na.rm=T))
  
  # join the extreme values to the statistics
  df_LT <- left_join(x = df_LT, y = dfExtreme, by="DOY")
  rm(dfExtreme)
  # create a new column with the date
  df_LT$Date <- as.Date("2022-12-31")+df_LT$DOY
  # 
  df_LT$Date <- ifelse(df_LT$Date > as.Date("2023-08-31"),
                       as.Date(paste(as.numeric(format(df_LT$Date,"%Y"))-1,
                       format(df_LT$Date,"%m"),format(df_LT$Date,"%d"),sep = "-")),
                       df_LT$Date)
  df_LT$Date <- as.Date(df_LT$Date, origin="1970-01-01")
  df_LT <- df_LT %>% arrange(Date)

  ###
  dfscen <- dfscen[,selcols]
  dfscen <- as.data.frame(dfscen)
  dfscen$param <- dfscen[,parameter]
  dfscen[,parameter] <- NULL
  df_LTscen <- dfscen %>% group_by(DOY) %>%
    summarise(param = quantile(param, c(0.25, 0.5, 0.75), na.rm=T), q = c(0.25, 0.5, 0.75)) %>%
    pivot_wider(id_cols = DOY, values_from = param, names_from = q, names_prefix = "q")
  dfExtremeScen <- dfscen %>% group_by(DOY) %>%
    summarise(max = max(param), min=min(param), mean=mean(param, na.rm=T))

  df_LTscen <- left_join(x = df_LTscen, y = dfExtremeScen, by="DOY")
  rm(dfExtremeScen)
  df_LTscen$Date <- as.Date("2022-12-31")+df_LTscen$DOY
  df_LTscen$Date <- ifelse(df_LTscen$Date > as.Date("2023-08-31"),
                       as.Date(paste(as.numeric(format(df_LTscen$Date,"%Y"))-1,
                                     format(df_LTscen$Date,"%m"),format(df_LTscen$Date,"%d"),sep = "-")),
                       #                           df_LT$Date-365,
                       df_LTscen$Date)




  df_LTscen$Date <- as.Date(df_LTscen$Date, origin="1970-01-01")
  df_LTscen <- df_LTscen %>% arrange(Date)



  ###
  y_label <- ifelse(ylabel=="", parameter, ylabel)

  if (SelYear==0) {
    ThisYear <- substr(as.character(Sys.Date()), 1,4)
    SelYear <- ThisYear
  }
#  df_LT$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LT$DOY
#  df_LTscen$Date <- as.Date(paste0(as.character(SelYear-1),"-12-31"))+df_LTscen$DOY

#  SelYearValues <- dfhist[dfhist$Year==SelYear, c( "Date","DOY", "param")]
#  SelYearValues <- as.data.frame(SelYearValues)
#  dfscenario <- dfscen
#  dfscenario <- dfscenario[,c( "Date","DOY", parameter)]


  p <- ggplot()
  p <- p + geom_ribbon(data = df_LT, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="green", alpha=0.3)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=max), color="green", size=0.5, lty=2)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=min), color="green", size=0.5, lty=2)
#  p <- p + geom_line(data=df_LT, aes(x=Date, y=q0.5), color="darkgreen", size=0.8, lty=1)
  p <- p + geom_line(data=df_LT, aes(x=Date, y=mean), color="darkgreen", size=0.8, lty=1)
  #  p <- p + geom_line(data=dfscenario, aes(x=Date, y=parameter), color="red", size=1)
  p <- p + geom_ribbon(data = df_LTscen, mapping = aes(x=Date, ymax=q0.75, ymin=q0.25), fill="red", alpha=0.4)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=max), color="red", size=0.5, lty=1)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=min), color="red", size=0.5, lty=1)
#  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=q0.5), color="red", size=1, lty=1)
  p <- p + geom_line(data=df_LTscen, aes(x=Date, y=mean), color="red", size=1, lty=1)
  p <- p + scale_x_date(labels = date_format("%b"), date_breaks = "1 month")
  p <- p + ylab(y_label)
  p <- p + theme_bw(base_size = BaseSize)
  p
  return(p)
}



# Radiation functions ----------------------------------------------


#' calcsolar: calculation of daily of maximum solar radiation
#'
#' @param dayofyear Julian day of the year.
#' @param latitude Latitude in °.
#'
#' @return Daily maximum global radiation in [MJ.m-2.d-1]
#' @export
#'
#' @examples
calcsolar <- function (dayofyear, latitude)
  {
    if (!is.numeric(dayofyear) || !is.numeric(latitude)) {
      stop("Invalid input. 'dayofyear' and 'latitude' should be numeric values.")
    }
    if (min(dayofyear) < 1 | max(dayofyear) > 366) {
      stop("Invalid input. 'dayofyear' should be a value between 1 and 366.")
    }
    if (min(latitude < -90) | max(latitude) > 90) {
      stop("Invalid input. 'latitude' should be a value between -90 and 90.")
    }
  rad <- pi/180
  dec <- -1 * asin(sin(23.45 * rad) * cos(2 * pi * (dayofyear +
                                                      10)/365))
  sinld <- sin(latitude * rad) * sin(dec)
  cosld <- cos(latitude * rad) * cos(dec)
  aob <- sinld/cosld
  dayl <- 12 * (1 + 2 * asin(aob)/pi)
  dsinb <- 3600 * (dayl * sinld + 24 * cosld * sqrt(1 - aob *
                                                      aob)/pi)
  sc <- 1370 * (1 + 0.033 * cos(2 * pi * dayofyear/365))
  angot <- sc * dsinb/1e+06
  return(list(declination = dec, daylength = dayl, angot.value = angot))
}



#'  estSg_S0: calculation of daily solar radiation from relative sunshine
#'
#' @param RelSun Relative sunshine duration of a particular day of year
#' @param Month Month of the year
#'
#' @return solar radiation per day [MJ/m2/d]
#' @details function for calculation of daily solar radiation from relative sunshine hours ####
#' the function uses an empirical model calibrated from DWD data "mod.Angstroem"
#' @examples
estSg_S0 <- function (RelSun, Month)
{
  if (is.numeric(Month)){Month <- as.integer(Month)}
  stopifnot(is.integer(Month))
  Month <- factor(Month, levels = as.character(1:12))
  mdf <- data.frame(RelSun = RelSun, Monat = Month)
  value <- predict.lm(object = mod.Angstroem, newdata = mdf)
  return(value)
}



#' RelRad: calculation of daily solar radiation from relative sunshine
#' the model has been fitted with observations from the DWD (relative sunshine and global radiation)
#' @param RelSun Relative sunshine duration of a particular day of year
#' @param Month Month of the year
#'
#' @return Global radiation in [MJ.m-2.d-1]
#' @export
#' @details New function for calculation of daily solar radiation from relative sunshine hours ####
#' the function uses directly the parameters  from  "mod.Angstroem"
#' @examples
RelRad <- function (RelSun=0.8, Month=6){

  
  # convert to integer
  Month <- as.integer(Month)
  
  # RelSun out of range transformed to NA
  # RelSun <- ifelse(RelSun < 0 | RelSun > 1, NA, RelSun)
  RelSun <- pmax(0, pmin(1, RelSun))
  
  Month <- as.integer(Month)
  
  sqrtRelSun  <- sqrt(RelSun)
  # value of the intercept  
  pIntercept  <- 0.161421912
  # linear effect for RelSun
  pRelSun     <- 0.107396393
  # quadratic effect for RelSun
  pSqrtRelSun <- 0.393292919
  # parameters for linear month effects
  pMonth      <- c(0, 0.007720969,0.001199219, -0.004020425, -0.012095101, -0.004887682, -0.009369806, -0.013787157,
               -0.009357840, -0.009234819, -0.011933316, -0.011964565)
  # parameters for interaction effect of RelSun and month
  pMonthRelSun <- c(0, 0.055166063, 0.119134736, 0.135627337, 0.113969224, 0.149730235, 0.136067936,
                    0.102568569, 0.093243449, 0.081928205, 0.013257431, -0.059281871)

  # parameters for interaction effect of sqrtRelSun and month
  pMonthSqrtRelSun <- c(0, -0.018514091, -0.043937525, -0.051414540, -0.024727021, -0.067051093,
                        -0.061308953, -0.034943014, -0.026797192, -0.026647466, 0.008849817,
                        0.042075264)

  value <- pIntercept+pMonth[Month]+ # intercept + month shifts
    RelSun*(pRelSun+pMonthRelSun[Month])+ # Interaction effect of RelSun
    sqrtRelSun*(pSqrtRelSun+pMonthSqrtRelSun[Month]) # Interaction effect of sqrtRelSun
  return(value)
}



# Wind speed correction ---------------------------------------------------


#' transWindSpeed
#' corrects the wind speed for the required height considering
#' the measurement height
#'
#' @param inWind original wind speed data [m/s]
#' @param inHeight measurement height of original wind speed data [m]
#' @param outHeight reference height to be wind speed should be converted [m]
#' @param vegi.height vegetation height [m]
#'
#' @return corrected wind speed data [m/s]
#' @export
#'
#' @examples
transWindSpeed<-function(inWind,inHeight,outHeight,vegi.height)
{

  # ra_f = (ln((inHeight-displHeight) / z0m) *
  #ln((inHeight-displHeight)/ z0h)/(sqr(Karman_const) * inWind)


  # ra_f = (ln((outHeight-displHeight) / z0m) *
  #ln((outHeight-displHeight)/ z0h)/(sqr(Karman_const) * outWind)

  Karman_const = 0.41
  # displHeight = 0.0804
  displHeight = 0.67*vegi.height
  # z0m = 0.01476
  z0m=0.123*vegi.height
  #Grasbestand
  z0h =0.1*z0m

  ## outWind/inWind = ra_f(outWind,outHeight) / ra_f(inWind, inHeight) =>
  #outWind = inWind*ra_f(outWind,outHeight) / ra_f(inWind, inHeight)

  outWind =
    inWind*(log((outHeight-displHeight)/z0m)*log((outHeight-displHeight)/z0h))/(log((inHeight-displHeight)/z0m)*log((inHeight-displHeight)/z0h))
  outWind

}



#' Alternative function for correction of windspeed to reference height
#'
#' @param ui original windspeed [m/s]] 
#' @param zi measurement height [m]
#' @param zo reference height [m]
#'
#' @return uo corrected windspeed [m/s]
#' @export
#'
#' @examples
windheight <- function(ui, zi, zo) {
  if (zo < 0.2 & zo > (5.42 / 67.8)) {
    warning("Wind-height profile function performs poorly when wind
            height is below 20 cm")
  }
  if (zo <= (5.42 / 67.8)) {
    warning(paste("wind-height profile cannot be calculated for height ",
                  zo * 100, " cm"))
    print("Height converted to 10 cm")
    zo <- 0.1
  }
  uo <- ui * log(67.8 * zo - 5.42) / log(67.8 * zi - 5.42)
  return(uo)
}


#  Handling of DWD files --------------------------------------------------

## Renaming of DWD files --------------------------------------------------


#' Copy_DWD_ZipFiles makes a local copy of DWD weather data as zip files
#'
#' @param DWD_ftp_ ftp address of DWD  weather data
#' @param LocalCopy_DWD_ftp_ directory for storing local copy of DWD data
#'
#' @return Nothing
#' @export
#'
#' @details can / should be used for recent and historical weather data
#' using the addresses of DWD_ftp_recent, DWD_ftp_historical and the directory names Local_DWD
#' @examples


#' rename weather data to long names
#'
#' @param DWDWeather Data frame in DWD name style.
#'
#' @return data frame with readable column names
#' @export
#'
#' @examples

rename.weather <- function(DWDWeather){
  stopifnot(names(DWDWeather)%in%c("Stations_id", "MESS_DATUM", "QN_3", "FX", "FM",
                                 "QN_4", "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", "UPM", "TXK",
                                 "TNK", "TGK", "eor",  "MHoeheWind", "Date", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname"))
  # rename columns
  for (i in 1:length(names_DWD)){
    oldname <- names_DWD[i]
    newname <- longNames_DWD[i]
    names(DWDWeather)[names(DWDWeather)==oldname] <- newname
  }

  return(DWDWeather)
}

#' abr.names shorten long column names
#'
#' @param tab Data frame for which columns should be renamed
#'
#' @return Data frame with shortened column names
#' @export
#'
#' @examples
abr.names <- function(tab){
  names(tab)[names(tab)=="LUFTTEMPERATUR"] <- "TMPM"
  names(tab)[names(tab)=="LUFTDRUCK_STATIONSHOEHE"] <- "LD"
  names(tab)[names(tab)=="REL_FEUCHTE"] <- "LF"
  names(tab)[names(tab)=="WINDGESCHWINDIGKEIT"] <- "Wind"
  names(tab)[names(tab)=="LUFTTEMPERATUR_MAXIMUM"] <- "TMPMX"
  names(tab)[names(tab)=="LUFTTEMPERATUR_MINIMUM"] <- "TMPMN"
  names(tab)[names(tab)=="NIEDERSCHLAGSHOEHE"] <- "Rain"
  names(tab)[names(tab)=="SONNENSCHEINDAUER"] <- "SunDur"
  return(tab)
}



#' rename.Rain renames the columns of the DWD rain data
#'
#' @param DWDRain data frame with original DWD rain data
#'
#' @return data frame with readable column names
#' @export
#'
#' @examples
rename.Rain <- function(DWDRain){
  stopifnot(names(DWDRain)==c("Stations_id", "MESS_DATUM","QN_6", "RS", "RSF","SH_TAG","NSH_TAG","eor",
                              "Date", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname"))
  # Umbenennen nach altem Schema
  names(DWDRain)[names(DWDRain)=="QN_6"] <- "QUALITAETS_NIVEAU"
  names(DWDRain)[names(DWDRain)=="RS"] <- "NIEDERSCHLAGSHOEHE"
  names(DWDRain)[names(DWDRain)=="NSH_TAG"] <- "NEUSCHNEEHOHE"
  names(DWDRain)[names(DWDRain)=="SH_TAG"] <- "SCHNEEHOHE"
  return(DWDRain)
}



## Get DWD metadata --------------------------------------------------



#' getDWDContent gets the content (filenames etc.) of a DWD ftp repository
#'
#' @param repository ftp address of repository with DWD weather data
#' @param quiet Give echo or not
#'
#' @return List with 1 data frame and one array of ZIPIDs
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#'
#' @examples
getDWDContent <- function(repository, quiet=T){
  # get the content/filenames of the DWD ftp repository
  filelist <- sort(stri_split_lines1(getURL(repository, dirlistonly = TRUE)))
  # select only the zip files
  ziplist <- grep(".zip", filelist, value=TRUE)
  # extract the 5 digit zipIDs
  zipID <- gsub("tageswerte_KL_", "", ziplist)
  zipID <- stri_split_fixed(zipID, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]
  path <- here()
  subDir <- "/DWD_tmp/"
  path <- file.path(path, subDir)
  dir.create(path = path, showWarnings = FALSE, recursive = T)
  dest_file <- paste0(path,  "Stationlist.txt")
  # Hier wird es kompliziert, da der DWD ständig das File-Format ändert...
  download.file(url=paste0(repository, "KL_Tageswerte_Beschreibung_Stationen.txt"),
                #		destfile="/DWD_tmp/Stationlist.txt", mode = "wb")
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")
  
  tmp <- stri_encode(stri_read_raw("DWD_tmp/Stationlist.txt"),
                     from="latin1", to="UTF-8")
  
  # Zeilenende ist irgendwas zwisch CRCRLF CRLF und LF... das sollte mit allem klappen...
  tmp <- stri_split_lines1(gsub("\r\n", "\n", tmp))
  
  # Trennzeichen und -breiten sind inkonsistent..
  tmp <- stri_split_regex(tmp,"[[:space:]]{1,}", omit_empty = TRUE)
  
  # In Stationsnamen kommen Trennzeichen vor... wieder zusammensetzen...
  
  tmplst <- sapply(tmp[3:length(tmp)],
                   FUN = function(obj){
                     if(length(obj)>8){
                       obj <- c(obj[1:6], paste(obj[7:(length(obj)-1)], collapse=" "), last(obj))
                     }
                     return(as.data.table(t(obj)))
                   }, simplify = FALSE)
  
  stationlist <- rbindlist(tmplst)
  names(stationlist) <- tmp[[1]]
  rm(tmp, tmplst)
  
  # some type conversion
  stationlist$Stationshoehe <- type.convert(stationlist$Stationshoehe, dec = ".", as.is="T")
  stationlist$geoBreite <- type.convert(stationlist$geoBreite, dec = ".", as.is="T")
  stationlist$geoLaenge <- type.convert(stationlist$geoLaenge, dec = ".", as.is="T")
  
  # Kodierung der ID mal mit und mal ohne führende Nullen. Das sollte der Schritt einheitlich machen...
  stationlist$Stations_id <- sprintf("%05i", type.convert(stationlist$Stations_id, dec = ".", as.is="T"))
  stationlist <- stationlist %>% filter(Stations_id %in% zipID)%>%
    distinct(Stations_id, .keep_all = T)
  # add columns for the start and end year of the data
  stationlist$bisJahr <- as.numeric(substr(x =stationlist$bis_datum, 1,4))
  stationlist$vonJahr <- as.numeric(substr(x =stationlist$von_datum, 1,4))
 
  # return the list of stationlist, filelist, ziplist and zipID
  return(list(stationlist= stationlist,
              filelist = filelist, 
              ziplist = ziplist, 
              zipID = zipID))
}



#' getDWDStationList gets a station list from the DWD ftp repository
#' without ziplists and addtionally returns historical and recent stationlist (metadat)
#' as well as filelists, ziplists and zipIDs
#'
#' @param historical ftp address of historical weather data
#' @param recent ftp address of recent weather data
#'
#' @return list of stations, recent, historical objects
#' stations: merged data frame of all weather stations with columns
#' "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe"
#' "geoBreite"     "geoLaenge" "Stationsname"  "Bundesland"
#' recent and historical: lists with
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#'
#' @examples
getDWDStationList <- function(historical, recent){
  historical <- getDWDContent(historical)
  recent <- getDWDContent(recent)
  #  stopifnot(all.equal(historical$stationlist, recent$stationlist))
  
  stations <- rbind(historical$stationlist, recent$stationlist)
  zipID.sum <- unique(c(historical$zipID, recent$zipID))
  #  zipID.sum <- zipID.sum[zipID.sum %in% historical$stationlist$Stations_id]
  tmp  <- stations %>% group_by(Stations_id) %>% 
    summarise(von_datum=min(as.integer(von_datum)),
              bis_datum=max(as.integer(bis_datum))) %>% 
    mutate(von_datum = as.character(von_datum),
           bis_datum = as.character(bis_datum))
  stations <- stations %>% dplyr::select(-von_datum, -bis_datum) %>% 
    distinct() 
  stations <- merge(x = tmp, y = stations, by="Stations_id")
  return(list(stations=stations, historical=historical, recent=recent))
}


#' getDWDRainContent gets the content of the DWD ftp repository for additional rain data
#'
#' @param repository ftp address of DWD repository for additional rain data
#' @param quiet option to echo off
#'
#' @return List with 1 data frame and one array of ZIPIDs
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs as characer vector
#' @export
#'
#' @examples
getDWDRainContent <- function(repository, quiet=T){
  filelist <- sort(stri_split_lines1(getURL(repository, dirlistonly = TRUE)))
  ziplist <- grep(".zip", filelist, value=TRUE)
  zipID <- gsub("tageswerte_RR_", "", ziplist)
  zipID <- stri_split_fixed(zipID, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]
  
  path <- here()
  subDir <- "/DWD_tmp/"
  
  dir.create(file.path(path, subDir), showWarnings = FALSE, recursive = T)
  
  dest_file <- paste(path,  "DWD_tmp/RainStationlist.txt", sep = "/")
  # Hier wird es kompliziert, da der DWD ständig das File-Format ändert...
  download.file(url=paste0(repository, "RR_Tageswerte_Beschreibung_Stationen.txt"),
                #		destfile="/DWD_tmp/Stationlist.txt", mode = "wb")
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")
  
  tmp <- stri_encode(stri_read_raw("DWD_tmp/RainStationlist.txt"),
                     from="latin1", to="UTF-8")
  
  # Zeilenende ist irgendwas zwisch CRCRLF CRLF und LF... das sollte mit allem klappen...
  tmp <- stri_split_lines1(gsub("\r\n", "\n", tmp))
  
  # Trennzeichen und -breiten sind inkonsistent..
  tmp <- stri_split_regex(tmp,"[[:space:]]{1,}", omit_empty = TRUE)
  
  # In Stationsnamen kommen Trennzeichen vor... wieder zusammensetzen...
  
  tmplst <- sapply(tmp[3:length(tmp)],
                   FUN = function(obj){
                     if(length(obj)>8){
                       obj <- c(obj[1:6], paste(obj[7:(length(obj)-1)], collapse=" "), last(obj))
                     }
                     return(as.data.table(t(obj)))
                   }, simplify = FALSE)
  
  stationlist <- rbindlist(tmplst)
  names(stationlist) <- tmp[[1]]
  rm(tmp, tmplst)
  
  stationlist$Stationshoehe <- type.convert(stationlist$Stationshoehe, dec = ".", as.is="T")
  stationlist$geoBreite <- type.convert(stationlist$geoBreite, dec = ".", as.is="T")
  stationlist$geoLaenge <- type.convert(stationlist$geoLaenge, dec = ".", as.is="T")
  # add columns for the start and end year of the data
  stationlist$bisJahr <- as.numeric(substr(x =stationlist$bis_datum, 1,4))
  stationlist$vonJahr <- as.numeric(substr(x =stationlist$von_datum, 1,4))
  
  # Kodierung der ID mal mit und mal ohne führende Nullen. Das sollte der Schritt einheitlich machen...
  stationlist$Stations_id <- sprintf("%05i", type.convert(stationlist$Stations_id, dec = ".", as.is="T"))
  stationlist <- stationlist %>% filter(Stations_id %in% zipID)
  return(list(stationlist= stationlist,
              filelist=filelist, ziplist=ziplist, zipID=zipID))
}




#' getDWDRainStationList gets a station list from the DWD ftp repository
#' without ziplists and addtionally returns historical and recent stationlist (metadata)
#' as well as filelists, ziplists and zipIDs
#'
#' @param historical ftp address of historical rain data
#' @param recent ftp address of recent rain data
#'
#' @return list of stations, recent, historical objects
#' stations: merged data frame of all weather stations with columns
#' "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe"
#' "geoBreite"     "geoLaenge" "Stationsname"  "Bundesland"
#' recent and historical: lists with
#' stationlist = data frame with the columns: "Stations_id"   "von_datum"     "bis_datum"     "Stationshoehe" "geoBreite"     "geoLaenge"
#' "Stationsname"  "Bundesland"
#' filelist: all filenames of the not zipped files repository
#' ziplist: names of the zip files in the repository
#' zipID: 5 digit ZipIDs
#' @export
#'
#' @examples
getDWDRainStationList <- function(historical, recent){
  historical <- getDWDRainContent(historical)
  recent <- getDWDRainContent(recent)
  #  stopifnot(all.equal(historical$stationlist, recent$stationlist))
  
  stations <- rbind(historical$stationlist, recent$stationlist)
  zipID.sum <- unique(c(historical$zipID, recent$zipID))
  #  zipID.sum <- zipID.sum[zipID.sum %in% historical$stationlist$Stations_id]
  tmp  <- stations %>% group_by(Stations_id) %>% 
    summarise(von_datum=min(as.integer(von_datum)),
              bis_datum=max(as.integer(bis_datum))) %>% 
    mutate(von_datum = as.character(von_datum),
           bis_datum = as.character(bis_datum))
  stations <- stations %>% dplyr::select(-von_datum, -bis_datum) %>% 
    distinct() 
  stations <- merge(x = tmp, y = stations, by="Stations_id")
  # add columns for the start and end year of the data
  stations$bisJahr <- as.numeric(substr(x =stations$bis_datum, 1,4))
  stations$vonJahr <- as.numeric(substr(x =stations$von_datum, 1,4))
  
  return(list(stations=stations, historical=historical, recent=recent))
}






#' Title
#'
#' @param dataperiod 
#' @param loadnew 
#' @param localStationdata_fn 
#'
#' @return
#' @export
#'
#' @examples
#' 
Get_ZipLists_Station_ids <- function(dataperiod="recent", loadnew=T, 
                                     localStationdata_fn = "stationdata.RData")  {
  
  
  stopifnot(dataperiod %in% c("recent", "historical", "both"))
  period <- dataperiod
  if (period %in% c("recent", "both")) {
    if (loadnew==T) {
      recent     <- getDWDContent(DWD_ftp_recent)
    } else
    {
      load(paste0(Local_R_DWD, localStationdata_fn))
    }
  }
  
  if (period %in% c("historical","both")) {
    if (loadnew==T) {
      historical <- getDWDContent(DWD_ftp_historical)
    } else
    {
      load(paste0(Local_R_DWD, localStationdata_fn))
    }
  }
  
  if (period == "recent"){
    ziplist <- recent$ziplist
    stationlist <- recent$stationlist %>% arrange(Stations_id, desc(bis_datum))
    historical <- NULL
  }  
  
  if (period == "historical"){
    ziplist <- historical$ziplist
    stationlist <- historical$stationlist %>% arrange(Stations_id, desc(bis_datum))
    recent <- NULL
  }  
  if (period == "both"){
    ziplist <- c(recent$ziplist, historical$ziplist)
    stationlist <- rbind(recent$stationlist,historical$stationlist)
    stationlist <- stationlist %>% arrange(Stations_id, desc(bis_datum))
  }  
  zipIDs <- gsub("tageswerte_KL_", "", ziplist)
  zipIDs <- unique(zipIDs)
  zipIDs <- stri_split_fixed(zipIDs, "_", n = 1, tokens_only=TRUE, simplify=TRUE)[,1]
  stationlist <- stationlist %>% filter(Stations_id %in% zipIDs)%>% distinct(Stations_id, .keep_all = T)
  return <- list(stationlist=stationlist, ziplist=ziplist, histziplist=historical$ziplist, recentziplist=recent$ziplist)
}



## Calculation of radiation from DWD weather data --------------------------------------------------

#' getRadWeather calculates radiation from DWD weather data
#'
#' @param WeatherTab Data frame with DWD data including sunshine duration without radiation
#'
#' @return Data frame with radiation in [MJ.m-2.d-1] and the other columns
#' @export
#' @details also saturation deficit is calculated
#'
#' @examples
getRadWeather <- function(WeatherTab) {
  # Validate input parameter
  if (!is.data.frame(WeatherTab)) {
    stop("Invalid input. 'WeatherTab' should be a data frame.")
  }

  # Create a copy of the input data frame
  RadWeather <- WeatherTab

  # Convert Date column to Date class and calculate DOY (day of year)
  RadWeather$Date <- as.Date(RadWeather$Date)
  RadWeather$DOY <- as.integer(format(RadWeather$Date, "%j"))
#  if ("sf" %in% class(RadWeather)) {
  
  # add geoLaenge and geoBreite as separate columns
  if (!is.null(RadWeather$geometry)) {
    RadWeather <- RadWeather %>%
      mutate(geoLaenge = unlist(map(RadWeather$geometry,1)),
             geoBreite = unlist(map(RadWeather$geometry,2)))
  }
  
  # Calculate solar parameters using calcsolar function
  RadWeather.Sun <- calcsolar(dayofyear = RadWeather$DOY, latitude = RadWeather$geoBreite)
  
  # calculate Dayl and Angot
  RadWeather$Dayl <- RadWeather.Sun$daylength
  RadWeather$Angot <- RadWeather.Sun$angot.value
  rm(RadWeather.Sun)

  # Calculate relative sunshine duration (RelSun)
  RadWeather$RelSun <- RadWeather$SONNENSCHEINDAUER / RadWeather$Dayl

  # Convert Date column to Month and factorize it
  RadWeather$Monat <- factor(month(RadWeather$Date), levels = 1:12)

  # Calculate estimated global radiation (EstGlobRad) using RelRad function
  RadWeather$EstSg_S0 <- RelRad(RadWeather$RelSun, as.integer(as.character(RadWeather$Monat)))
  RadWeather$EstGlobRad <- RadWeather$EstSg_S0 * RadWeather$Angot

  # Convert Date column to Excel time representation
  RadWeather$ExcelTime <- as.integer(difftime(RadWeather$Date, as.Date("1899-12-30")))

  # Calculate VP (Vapour Pressure) and Sat_def (Saturation Deficit)
  RadWeather$VP <- 6.1078 * exp(17.2694 * RadWeather$LUFTTEMPERATUR / (RadWeather$LUFTTEMPERATUR + 238.3)) * RadWeather$REL_FEUCHTE / 100
  RadWeather$Sat_def <- 6.1078 * exp(17.2694 * RadWeather$LUFTTEMPERATUR / (RadWeather$LUFTTEMPERATUR + 238.3)) * (1 - RadWeather$REL_FEUCHTE / 100)

  # Calculate Rad_Int (Integrated Radiation)
  RadWeather$Rad_Int <- RadWeather$EstGlobRad * 1000 * 1000 / (24 * 60 * 60)

  # Select desired columns
  selcols <- c("Stations_id",  "WINDGESCHWINDIGKEIT", "NIEDERSCHLAGSHOEHE",#"MESS_DATUM",
               "SONNENSCHEINDAUER", "LUFTTEMPERATUR", "REL_FEUCHTE",
               "LUFTTEMPERATUR_MAXIMUM", "LUFTTEMPERATUR_MINIMUM", "MHoeheWind",
               "Date", "Stationshoehe", "geoBreite", "geoLaenge",
               "Stationsname", "DOY", "Dayl", "Angot",
               "RelSun", "Monat", "EstSg_S0", "EstGlobRad", "ExcelTime",
               "VP", "Sat_def", "Rad_Int")
  
  # Return the data frame with selected columns
  RadWeather <- as.data.frame(RadWeather)
  RadWeather <- RadWeather[,selcols]# RadWeather[, selcols, drop = FALSE]

  return(RadWeather)
}

## Download functions for DWD weather data --------------------------------------------------


#' Copy_DWD_ZipFiles makes a local copy of DWD weather data as zip files
#'
#' @param DWD_ftp_ ftp address of DWD  weather data
#' @param LocalCopy_DWD_ftp_ directory for storing local copy of DWD data
#'
#' @return Nothing
#' @export
#'
#' @examples
Copy_DWD_ZipFiles <- function(DWD_ftp_, LocalCopy_DWD_ftp_) {
  
  # copy all DWD files, (in particular zip files from the ftp server to the local directory
  # DWD_ftp_ = "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
  # LocalCopy_DWD_ftp_ = "C:/Users/.../LocalCopyDWD/recent/"
  # DWD_ftp_ = "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
  # LocalCopy_DWD_ftp_ = "C:/Users/.../LocalCopyDWD/historical/"
  
  # check if the local directory exists, if not create it  
  if (!dir.exists(LocalCopy_DWD_ftp_)) {
    dir.create(LocalCopy_DWD_ftp_, recursive = T)}
  
  # Validate input parameters 
  if (!is.character(DWD_ftp_) || !is.character(LocalCopy_DWD_ftp_)) {
    stop("Invalid input. 'DWD_ftp_' and 'LocalCopy_DWD_ftp_' should be character strings.")
  }
  # Set user agent for the HTTP request
  user_agent("R User Agent")
  
  # Create a new handle with anonymous user credentials
  h <- new_handle()
  handle_setopt(h, userpwd = "anonymous")
  
  # Check if the FTP repository URL ends with a slash, add if missing
  if (!str_ends(DWD_ftp_, "/")) {
    DWD_ftp_ <- paste0(DWD_ftp_, "/")
  }
  
  # Get the list of file names from the FTP directory
  tryCatch({
    response <- GET(DWD_ftp_, dirlistonly = TRUE)
    #  #  if (http_type(response) != "text/html") {
    #  #    stop("Failed to retrieve file list from FTP server.")
    #  #  }
    filelist <- sort(stri_split_lines1(getURL(DWD_ftp_, dirlistonly = TRUE)))
  }, error = function(e) {
    stop("Failed to retrieve file list from FTP server.")
  })
  
  # Download each file from the FTP server to the local directory
  for (i in seq_along(filelist)) {
    filename <- filelist[i]
    savedir <- here()
    newdir <-  LocalCopy_DWD_ftp_
    setwd(newdir)
    dest_file <- filename
    cat(i, "\n")
    if (nchar(dest_file) > 1) {
      tryCatch({
        curl_download(paste0(DWD_ftp_, filename), destfile = dest_file, handle = h, quiet = FALSE)
      }, error = function(e) {
        warning(paste("Failed to download file:", filename))
      })
    }
    setwd(savedir)
  }
}



#' UpdateDWDData_to_fst updates the DWD weather data to a local fst file
#'
#' @param dataperiod choose recent or historical weather data
#' @param startdate start date for the weather data
#' @param isloadnew option to load data from DWD ftp server 
#' @param DWD_content list with stationlist, ziplist, zipID
#'
#' @return Nothing, a fst file is written to the local directory
#' @export
#'
#' @examples
#' 
#' 

UpdateDWDData_to_fst <- function(dataperiod="recent", startdate="1990-01-01", isloadnew=T, DWD_content=NULL) {
  
  # check if dataperiod parameter is valid
  stopifnot(dataperiod %in% c("recent", "historical"))
  startyear <- year(as.Date(startdate))
  
  ### load station data if not given as parameter
  if (is.null(DWD_content)){
    DWD_content <- getDWDStationList(historical = DWD_ftp_historical, 
                                     recent = DWD_ftp_recent)
  }
  
  # use recent or historical data
  if (dataperiod == "recent") {
    stationlist <- DWD_content$recent$stationlist
    stationlist <- stationlist %>% distinct(Stations_id, .keep_all = T)
    Stationids  <- substr(DWD_content$recent$ziplist, 15, 19)
    ziplist <- DWD_content$recent$ziplist
  }
  
  if (dataperiod == "historical") {
    stationlist <- DWD_content$historical$stationlist
    stationlist <- stationlist %>% distinct(Stations_id, .keep_all = T)
    Stationids  <- substr(DWD_content$historical$ziplist, 15, 19)
    ziplist <- DWD_content$historical$ziplist
  }
  
  # length of the station list
  n_df <- length(Stationids)
  
  # prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)
  
  ## loop over the stations and get the weather data
  if (dataperiod == "recent") {
    for (i in 1:length(Stationids)){
      #  i <- 18
      station <- Stationids[i]
      df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_recent, local = !isloadnew), silent = F)
    }
  }
  # same for historical data
  if (dataperiod == "historical") {
    for (i in 1:length(Stationids)){
      #  i <- 18
      station <- Stationids[i]
      #  df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_historical, local = isloadnew), silent = F)
      df_list[[i]] <- getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_historical, local = F)
    }
  }
  
  # remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  
  # convert list of data frames in one
  df <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  
  #  df$Date <- NULL
  df$Date <- ymd(df$MESS_DATUM)
  df <- df %>% dplyr::filter(Date >= as.Date(startdate))
  
  # rename columns
  df <- rename.weather(df)
  
  # retrieve stations_id from filename
  df$Stations_id <- sprintf("%05i", type.convert(df$Stations_id, dec = ".", as.is="T"))
  df <- df %>% left_join( stationlist, by = "Stations_id")
  class(df) <- class(as.data.frame(df))
  
  # add estimates for radiation 
  df <- getRadWeather(df)
  #  df <- df %>% select( -FX, -RSKF, -SHK_TAG, -NM, -VPM,-eor, -TGK)
  fn <- paste0("weather_dat_",as.character(startyear),".fst")
  
  # save data to fst file
  if (dataperiod == "recent") {
    write.fst(x = df, path = paste0(Local_R_DWD,"weather_recent_dat.fst"))}
  if (dataperiod == "historical") {
    write.fst(x = df, path = paste0(Local_R_DWD, fn))}
}




#' Title getsingleDWDWeather loads data for a single weather station
#' 
#' @details This is a function which loads data for a single weather station either for
#' recent or historical weather data. The data are retrieved in the DWD format.
#' The measurement height for wind speed is taken from the meta data of the station and
#' added to the data frame
#'
#' @param station 5 digit ID of the weather station as character
#' @param ziplist Character array with the ZIP-Filenames
#' @param repository ftp Repository address or local directory
#' @param local Option to use local copy or ftp data
#' @param quiet echo on/off
#'
#' @return data frame with weather data from DWD for either historical or recent zip files
#' @export
#'
#' @examples
getsingleDWDWeather <- function(station, ziplist, repository, local=F, quiet=T){

  # if tmp directory is not existing, create it
  path <- paste0(here(),"/DWD_tmp/")
  if (dir.exists(paste0(path))==F){
    dir.create(path, recursive = T)
  }
  
  # if unzip directory is not existing, create it
  path_unzip <- paste0(path,"unzip/")
  if (dir.exists(paste0(path_unzip))==F){
    dir.create(path_unzip, recursive = T)
  }
  
  # name of file with measurement height of wind data 
  df_mHeight <- NULL
  #  filename <-
  
  # select the file name for to be zipfile to be downloaded/unzipped from ziplist
  filename <- grep(paste0("tageswerte_KL_", station), ziplist, value = TRUE)
  if (nchar(filename)<2) {
    stop()
    return <- NULL
    }
  # filename <- paste0(path, "/", filename)
  # name of the destination file
  dest_file <- paste0(path, filename)
  
  # if not local  download the zip file from the ftp server
  if (!local) {
    download.file(url=paste0(repository, filename),
                destfile=dest_file, mode = "wb", quiet = quiet, method = "auto")} 
  
  # check if path_unzip exists, if yes remove it
  if(dir.exists(path_unzip)){
    unlink(path_unzip, recursive = TRUE, force = TRUE)
  }
  
  # if local, unzip the file from the local directory
  if (local){
    # check if the file exists in the local directory
    if (file.exists(paste0(repository, filename))){
      unzip(zipfile = paste0(repository, "/",filename), 
            exdir = path_unzip)
      filename <- grep("produkt_klima_tag_", list.files(path_unzip), value = TRUE)}
    # if not, stop the function
    else {
      return <- NULL
      stop()
      }
  } 
  # if not local, unzip the file from the temporary directory
  else {
    unzip(paste0(path, filename), exdir = path_unzip)
    # get the name of the unzipped file with the weather data
    filename <- grep("produkt_klima_tag_", list.files(path_unzip), value = TRUE)
  }
  
  # read weather data from temporary file
  DWDWeather <- read.table(file = paste0(path_unzip, filename),
                           header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                           stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  
  # add standard measurement height for wind speed
  DWDWeather$MHoeheWind <- 10 # default value
  DWDWeather$Date <- as.Date(as.character(DWDWeather$MESS_DATUM), "%Y%m%d")
  
  # read meta data for wind speed measurement height
  MetaWindfn <- paste0("Metadaten_Geraete_Windgeschwindigkeit_", station,".txt")
  if(file.exists(paste0(path_unzip, MetaWindfn))){
  MetaWinddata <- read.table(file = paste0(path_unzip, MetaWindfn),
                             header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                             stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  # letzte Zeile raus
  MetaWinddata <- MetaWinddata[-nrow(MetaWinddata),]
  MetaWinddata <- MetaWinddata %>% mutate(Von_Datum = as.Date(as.character(Von_Datum), format="%Y%m%d"),
                                         Bis_Datum=as.Date(as.character(Bis_Datum), format="%Y%m%d"))
  
  # extract the measurement height for wind speed
  dateseq <- NULL#Date()
  Heightseq <- NULL
  for (i in 1:nrow(MetaWinddata)){
    dateseqi <- seq(MetaWinddata[i, "Von_Datum"], MetaWinddata[i, "Bis_Datum"], "1 day" )
    dateseq <- c(dateseq, dateseqi)
    Heightseqi <- rep(MetaWinddata[i, "Geberhoehe.ueber.Grund..m."], length(dateseqi))
    Heightseq <- c(Heightseq, Heightseqi)

  }
  df_mHeight <- data.frame(Date=as.Date(x = dateseq, origin="1970-01-01"), MHoeheWind=Heightseq)
  df_mHeight <- df_mHeight %>% distinct(Date, .keep_all = T)
}
  
  # remove temporary files
  if(file.exists(path_unzip)){
    unlink(path_unzip, recursive = TRUE, force = TRUE)
  }
  
  # add measurement height for wind speed to the data frame
  if (!is.null(df_mHeight)){
    DWDWeather$MHoeheWind <- NULL
    DWDWeather <- DWDWeather %>% left_join(x = DWDWeather, y = df_mHeight, by="Date")
  }
  
  DWDWeather <- DWDWeather %>% dplyr::rename(Stations_id = STATIONS_ID)
  return(DWDWeather)
}



#' getsingleDWDRain loads rainfall data for a single weather station
#'
#' @param station 5 digit ID of the weather station as character
#' @param ziplist Character array with the ZIP-Filenames
#' @param repository ftp Repository address or local directory
#' @param local Option to use local copy or ftp data
#' @param quiet echo on/off
#'
#' @return data frame with rain data from DWD for either historical or recent zip files
#' @export
#'
#' @examples
getsingleDWDRain <- function(station, ziplist, repository, local=F, quiet=T){
  path <- here()
  filename <- grep(paste0("tageswerte_RR_", station), ziplist, value = TRUE)
  # filename <- paste0(path, "/", filename)
  dest_file <- paste0(path,"/DWD_tmp/", filename)
  if (!dir.exists(path)) {dir.create(path, recursive = T)}
  if (!local) {
    
    download.file(url=paste0(repository, filename),
                  destfile=dest_file, mode = "wb", method = "auto", quiet)}else{
                    
                  }
  
  
  if(file.exists("DWD_tmp/unzip")){
    unlink(paste0(path,"/DWD_tmp/unzip"), recursive = TRUE, force = TRUE)
  }
  
  if (local){
    if (file.exists(paste0(repository, "/",filename))){
      unzip(paste0(repository, "/",filename), exdir = paste0(path,"/DWD_tmp/unzip"))
      filename <- grep("produkt_nieder_tag_", list.files(paste0(path,"/DWD_tmp/unzip/")), value = TRUE)}
    else {
      return <- NULL
      stop()
    }
  } else {
    unzip(paste0(path,"/DWD_tmp/", filename), exdir = paste0(path,"/DWD_tmp/unzip"), overwrite = T)
    filename <- grep("produkt_nieder_tag_", list.files(paste0(path,"/DWD_tmp/unzip/")), value = TRUE)
  }
  
  
  #  unzip(paste0(path,"/DWD_tmp/", filename), exdir = paste0(path,"/DWD_tmp/unzip"))
  #  filename <- grep("produkt_nieder_tag_", list.files(paste0(path,"/DWD_tmp/unzip/")), value = TRUE)
  
  DWDWeather <- read.table(file = paste0(path,"/DWD_tmp/unzip/", filename),
                           header=TRUE, sep=";", quote="", dec=".", na.strings=c("-999", "\032"),
                           stringsAsFactors=FALSE, strip.white=TRUE, fill=TRUE, fileEncoding="latin1")
  DWDWeather$Date <- as.Date(as.character(DWDWeather$MESS_DATUM), "%Y%m%d")
  if(file.exists(paste0(path,"/DWD_tmp/unzip"))){
    unlink(paste0(path,"/DWD_tmp/unzip"), recursive = TRUE, force = TRUE)
  }
  return(DWDWeather)
}



#' getcombinedDWDWeather gets the combined (historic & recent) weather data for a DWD station
#'
#' @param DWD_content A list structure of the DWD stations
#' @param station The ID of the station in 5 digits but as character
#' @param local Option to use local stored zip-files
#' @param recent_repository The ftp address of the recent zip files or the local directory
#' @param historical_repository The ftp address of the historical zip files or the local directory
#'
#' @return data frame with DWD observations historical + recent
#' @export
#'
#' @examples
getcombinedDWDWeather <- function(DWD_content, station, local=F,
                                  recent_repository, historical_repository){
  # stattion ID to character
  station <- as.character(station)
  if(station %in% DWD_content$historical$zipID){
    # if the station is in the historical zipID list, get the historical data
    data.hist <- getsingleDWDWeather(station, DWD_content$historical$ziplist, repository = historical_repository, local)
  }
  if(station %in% DWD_content$recent$zipID){
    # if the station is in the recent zipID list, get the recent data
    data.rct <- getsingleDWDWeather(station, DWD_content$recent$ziplist, repository = recent_repository, local)
  }

  if(exists("data.hist") & exists("data.rct")){
    # detect the last date of the historical data
    t1 <- data.hist$Date[length(data.hist$Date)]
    # combine the historical and recent data filtered first by the last date of the historical data
    data.sum <- rbind(data.hist, data.rct[data.rct$Date > t1,])
  }else{
    if(exists("data.hist")){
      # if only historical data is available
      data.sum <- data.hist
    }else{
      if(exists("data.rct")){
      # if only recent data is available
      data.sum <- data.rct
      }
    }
  }
  if (exists("data.sum")) {
    # add the meta data of the station to the data frame
    data.sum <- cbind(data.sum,
                    DWD_content$stations[DWD_content$stations$Stations_id==station, c("Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")])
    # remove empty last row
    data.sum <- subset(data.sum, !is.na(Stations_id))
  }else{
    data.sum <- NA
    }
  return(data.sum)
}


#' getcombinedDWDRain gets the combined (historic & recent) rain data for a DWD station
#'
#' @param DWD_content A list of the DWD station
#' @param station The ID of the station in 5 digits but as character
#' @param local Option to use local stored zip-files
#' @param recent_repository The ftp address of the recent zip files or the local directory
#' @param historical_repository The ftp address of the historical zip files or the local directory
#'
#' @return data frame with DWD observations historical + recent
#' @examples
getcombinedDWDRain <- function(DWDRain_content, station, local=F,
                               recent_repository, historical_repository){
  station <- as.character(station)
  if(station %in% DWDRain_content$historical$zipID){
    # if the station is in the historical zipID list, get the historical data  
    data.hist <- getsingleDWDRain(station, DWDRain_content$historical$ziplist, DWDRain_ftp_historical)
  }
  if(station %in% DWDRain_content$recent$zipID){
    # if the station is in the recent zipID list, get the recent data
    data.rct <- getsingleDWDRain(station, DWDRain_content$recent$ziplist, DWDRain_ftp_recent)
  }

  if(exists("data.hist") & exists("data.rct")){
    # detect the last date of the historical data
    t1 <- data.hist$Date[length(data.hist$Date)]
    # combine the historical and recent data filtered first by the last date of the historical data
    data.sum <- rbind(data.hist, data.rct[data.rct$Date > t1,])
  }else{
    # if only historical data is available
    if(exists("data.hist")){
      data.sum <- data.hist
    }else{
      # if only recent data is available
      if(exists("data.rct")){
        data.sum <- data.rct
      }
    }
  }
  if (exists("data.sum")) {
    DWDRain_content$stations$Stations_id <- as.integer(DWDRain_content$stations$Stations_id)
#    data.sum <- cbind(data.sum,
#                      DWDRain_content$stations[DWDRain_content$stations$Stations_id==station, c("Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")])
    SStations <- DWDRain_content$stations
    #    data.sum <- merge(x = data.sum, y=as.data.frame(DWDRain_content$stations[,c("Stations_id","Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")]),
    #                  by.x="Stations_id",by.y="Stations_id")
    
    # rename the column name of the station id
    if ("STATIONS_ID" %in% names(data.sum)) {
      names(data.sum)[names(data.sum)=="STATIONS_ID"] <- "Stations_id"}
    
    # add the meta data of the station to the data frame
    data.sum <- merge(x = data.sum, y=SStations[,c("Stations_id","Stationshoehe", "geoBreite", "geoLaenge", "Stationsname")],
                      by="Stations_id")
    # remove empty last row
    data.sum <- subset(data.sum, !is.na(Stations_id))
  }else{
    data.sum <- NA
  }
  return(data.sum)
}



#' SelectStations selects the nearest weather stations from a list of stations
#'
#' @param lat Latitude of location
#' @param long Longitude of location
#' @param height_loc Height of the location
#' @param stationlist List of stations from which the stations should be selected
#' @param minstations minimum number of stations to be selected
#' @param radius Maximum radius where the station should be selected from
#' @param startdate date from which weather data should be available
#' @param max.Height.Distance_m maximum height
#'
#' @return data frame with DWD station info for selected stations
#' @export
#'
#' @examples
SelectStations <- function(lat, long, height_loc=100, stationlist, minstations=7,
                           max_stations=20,
                           radius=70000,  startdate, max.Height.Distance_m=100) {

# create a data frame from location coordinates
  Standorte_list <- data.frame(geoBreite=lat, geoLaenge=long)
  standorte_list <- st_as_sf(Standorte_list, coords = c("geoLaenge", "geoBreite")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

# make an sf object from stationlist data frame
  stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")

# calculate distances between stations and location
  stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, standorte_list))) # minimum distance is 1 m because
  stationlist$Distance_km <- format(stationlist$Distance_m /1000, digits = 3)

# filter
  stationlist <- stationlist %>%
    mutate("Height.Distance_m" = Stationshoehe - height_loc) %>%
    filter(Distance_m < radius,
           bis_datum > startdate,
           Height.Distance_m < abs(max.Height.Distance_m)) %>%
    arrange(Distance_m)

  # filter version with nearest minstations stations
  Minstation_list <- stationlist %>% arrange(Distance_m) %>% slice_head(n=minstations)

  # filter version with nearest according to radius
  Nearstation_list <- stationlist %>% filter(Distance_m<=radius)

  # select the list with the minimum number of stations if its larger 
  # than the list with the nearest stations
  if(nrow(Minstation_list)>nrow(Nearstation_list))
    {stations_selected <- Minstation_list} else
  {stations_selected <- Nearstation_list}
  
  # if the number of stations is larger than max_stations, select the nearest max_stations
  if(nrow(stations_selected)>max_stations){
    stations_selected <- Nearstation_list %>% slice_head(n=max_stations)
  }
  rm(Nearstation_list, Minstation_list)
  # add geoLaenge and geoBreite as columns
  stations_selected <- stations_selected %>%
    mutate(geoLaenge = unlist(map(stations_selected$geometry,1)),
           geoBreite = unlist(map(stations_selected$geometry,2)))
#  st_geometry(stations_selected) <- NULL
  return (stations_selected)
}



#' GetWeatherData_selection gets the weather data for the selected stations
#' by downloading the data from the DWD ftp server
#' @param stations_selected selected stations
#' @param DWD_content list structure of available DWD stations
#' @param local option for using local stored DWD zip-Files
#' @param startdate start date for selection of data
#'
#' @return
#' @export
#'
#' @examples
GetWeatherData_selection <- function(stations_selected,  
                                     DWD_content, 
                                     repository=DWD_ftp_historical,
#                                     local = F,
                                     startdate="1990-01-01") {

  # Use local data?
  local <- ifelse(repository %in% c(DWD_ftp_recent, DWD_ftp_historical), F, T)
  
  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  weatherdata <- NULL
  # loop over selected stations
  for (station in stations_selected$Stations_id) {
    # for debugging
    #    ID <- station_selected$Stations_id[1]
    # is station in the list of available zip files?
    if (station %in% DWD_content$zipID){
      
      # get the weather data for the station
      weather <- getsingleDWDWeather(station = station, 
                                     ziplist = DWD_content$ziplist,
                                     repository = repository, 
                                     local = local, 
                                     quiet = T)
      weather$Stations_id <- station
      # add to list of data frames
      df_list[[station]] <- weather}
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one
  # convert list of data frames in one 
  weatherdata <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  weatherdata <- as.data.frame(weatherdata)
  # remove data older than start date
  weatherdata <- weatherdata %>% filter(Date >= as.Date(startdate))
  # rename columns
  weatherdata <- rename.weather(weatherdata)
  # add station info
  weatherdata <- weatherdata %>% left_join( stations_selected, by = "Stations_id")
  # add radiation data
  weatherdata <- getRadWeather(weatherdata)
  stations_selected$Distance_km <- stations_selected$Distance_m / 1000
  # calculate wind speed at 10 m
  weatherdata$WINDGESCHWINDIGKEIT <-  windheight( ui = weatherdata$WINDGESCHWINDIGKEIT,
                                                     zi =  weatherdata$MHoeheWind, zo = 10)
  weatherdata <- merge(x = weatherdata, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  weatherdata <- weatherdata %>% mutate(Jahr=year(Date), Monat=month(Date),
                                              ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", "MHoeheWind", all_of(measvars)))


  return(weatherdata)
}




#' GetWeatherData_selection_fst 
#'
#' @param stations_selected selected stations
#' @param DWD_content list structure of available DWD stations
#' @param local option for using local stored DWD zip-Files
#' @param startdate start date for selection of data
#'
#' @return
#' @export
#'
#' @examples
GetWeatherData_selection_fst <- function(stations_selected,  DWD_content, repository=DWD_ftp_recent,
                                         startdate="1990-01-01") {


  local <- ifelse(repository == DWD_ftp_recent, F, T)
  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)

  weather_recent <- NULL
  #file.name = file.names[3]
  #  station_selected$Stations_id <-  sprintf("%05i", type.convert(station_selected$Stations_id, dec = "."))

  #  EndDate <- Sys.Date()
  #  Length <- EndDate-as.Date(startdate)+1

  for (station in stations_selected$Stations_id) {
    #    ID <- station_selected$Stations_id[1]
    if (station %in% DWD_content_recent$zipID){
    weather <- getsingleDWDWeather(station = station, ziplist = DWD_content$recent$ziplist,
                                     repository = repository, local = local, quiet = T)
    weather$Stations_id <- station
    df_list[[station]] <- weather}
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one

  weather_recent <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  weather_recent <- as.data.frame(weather_recent)
  weather_recent <- weather_recent %>% filter(Date >= as.Date(startdate))
  weather_recent <- rename.weather(weather_recent)
  weather_recent <- weather_recent %>% left_join( stations_selected, by = "Stations_id")
  weather_recent <- getRadWeather(weather_recent)
  weather_recent$WINDGESCHWINDIGKEIT <-  windheight( ui = weather_recent$WINDGESCHWINDIGKEIT,
                                                  zi =  weather_recent$MHoeheWind, zo = 10)
  weather_recent <- merge(x = weather_recent, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  weather_recent <- weather_recent %>% mutate(Jahr=year(Date), Monat=month(Date),
                                        ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", all_of(measvars)))

  weather_historical <- read.fst("./LocalCopyDWD/Rdata/weather_dat_1990.fst")
  weather_historical$Jahr <- year(weather_historical$Date)
  selcols <- names(weather_recent)
  weather_historical <- weather_historical[,selcols]
  weather_all <- rbind(weather_historical, weather_recent)

  return(weather_all)
}



#' getHUMEWeather converts the DWD weather data into the HUME format
#'
#' @param RadWeather Data frame with augmented DWD data
#'
#' @return Data frame in HUME weather format
#' @export
#'
#' @examples
getHUMEWeather <- function(RadWeather){
  HUMEWeather <- RadWeather
  HUMEWeather <- subset(HUMEWeather, select = c(ExcelTime, LUFTTEMPERATUR, NIEDERSCHLAGSHOEHE, REL_FEUCHTE,
                                                VP, Sat_def, Rad_Int, EstGlobRad, WINDGESCHWINDIGKEIT, LUFTTEMPERATUR_MINIMUM, LUFTTEMPERATUR_MAXIMUM))
  HUMEWeather$LUFTTEMPERATUR <- round(HUMEWeather$LUFTTEMPERATUR, 1)
  HUMEWeather$NIEDERSCHLAGSHOEHE <- round(HUMEWeather$NIEDERSCHLAGSHOEHE, 1)
  HUMEWeather$REL_FEUCHTE <- round(HUMEWeather$REL_FEUCHTE, 1)
  HUMEWeather$VP <- round(HUMEWeather$VP, 1)
  HUMEWeather$Sat_def <- round(HUMEWeather$Sat_def, 1)
  HUMEWeather$Rad_Int <- round(HUMEWeather$Rad_Int, 1)
  HUMEWeather$EstGlobRad <- round(HUMEWeather$EstGlobRad, 2)
  HUMEWeather$WINDGESCHWINDIGKEIT <- round(HUMEWeather$WINDGESCHWINDIGKEIT, 1)
  HUMEWeather$LUFTTEMPERATUR_MINIMUM <- round(HUMEWeather$LUFTTEMPERATUR_MINIMUM, 1)
  HUMEWeather$LUFTTEMPERATUR_MAXIMUM <- round(HUMEWeather$LUFTTEMPERATUR_MAXIMUM, 1)
  names(HUMEWeather) <- c("Time", "TMPM", "Rain", "LF", "VP", "Sat_def", "Rad_Int", "GlobRad", "Wind", "TMPMN", "TMPMX")
  return(HUMEWeather)
}




#' LoadDWDDatafromStationlist
#'
#' @param station_selected # a data frame with selected stations
#' @param RainStation_selected # a data frame with selected rain stations
#' @param Max_Height_diff <- 100 # maximum height difference (m)
#' between location and station to be accepted
#' @param weather_historic # a data frame with historical weather data
#' @param weather_recent # a data frame with recent weather data
#' @param StartYear # start year for data selection
#' @param maxError # maximum deviation of monthly parameter value 
#' from average of stations as multiples of standard deviation
#' @param Max_Height_diff # maximum difference from location to station to which stations are included
#'
#' @return interpolated weather data in HUME format including radiation data
#' @export
#'
#' @examples
#' 
  LoadDWDDatafromStationlist <- function(station_selected,
                                       RainStation_selected=NULL,
                                       DWD_content=NULL,
                                       DWDRain_content=NULL,
                                       weather_historic=NULL,
                                       weather_recent=NULL,
                                       local = F,
                                       StartYear= 1990,
                                       maxError = 2,
                                       Max_Height_diff = 100)
{
  
  startdate <- paste0(as.character(StartYear), "-01-01")
  
  # if no meta data are given, load them from DWD
  if (is.null(DWD_content)){
    DWD_content <- getDWDStationList(historical = DWD_ftp_historical, recent = DWD_ftp_recent)
  }
  
  # if no meta data for additional rainfall stations are given, load them from DWD
  if (is.null(DWDRain_content)){
    DWDRain_content <- getDWDRainContent(repository = DWDRain_ftp_recent)
  }  
  

  # AddRainstations
  
  if (!is.null(RainStation_selected)){
    if (is.null(DWDRain_content)){
      DWDRain_content <- getDWDRainStationList(recent = DWDRain_ftp_recent, historical = DWD_ftp_historical)
    }
    # select for stations which are not in selected general weather stations  
#    RainStation_selected <- RainStation_selected[!(RainStation_selected$Stations_id %in% station_selected$Stations_id),]
  }
  
  
  # if no historical weather data are given, load them from DWD  
  if (is.null(weather_historic)) {
    
    
    weather_historic <- GetWeatherData_selection(stations_selected = station_selected,
                                                 DWD_content = DWD_content$historical,
#                                                 local = local, 
                                                 repository = DWD_ftp_historical, 
                                                 startdate = startdate)
  } else 
  {
    weather_historic <- weather_historic %>% filter(Date >= startdate)
    weather_historic <- weather_historic %>% filter(Stations_id %in% station_selected$Stations_id)
    #    weather_historic <- setDT(weather_historic)
  }  
  # if no recent weather data are given, load them from DWD
  if (is.null(weather_recent))  {
    weather_recent <- GetWeatherData_selection(stations_selected =  station_selected,
                                          DWD_content$recent, 
                                          repository=DWD_ftp_recent,
                                          startdate=startdate)
  } else {
    # convert station id to character     
    weather_recent$Stations_id <- sprintf("%05i", type.convert(weather_recent$Stations_id, dec = ".", as.is="T"))
    
    # filter recent weather data for selected stations  
    weather_recent <- weather_recent %>%  filter(Stations_id %in% station_selected$Stations_id)
  }
  
  
  # combine historical and recent data
  
  weather_historic$Jahr <- year(weather_historic$Date)
  weather_recent$Jahr <- year(weather_recent$Date)
  
  selcols <- names(weather_recent)
  weather_all <- rbind(weather_historic[, ..selcols], weather_recent)
  rm(weather_recent, weather_historic)
  
  
  # download the additional rain data for the local stations near the point of interest
  AddRainData <- NULL
  for (ID in RainStation_selected$Stations_id) { 
    rain <- getcombinedDWDRain(DWDRain_content =  DWDRain_content, 
                               historical_repository = DWDRain_ftp_historical, recent_repository = DWDRain_ftp_recent,
                               station = ID)
    if (!is.null(rain)){if(class(rain)=="data.frame"){
      AddRainData <- dplyr::bind_rows(AddRainData, rain)
    }}
  }
  # filter for data newer than start date
  AddRainData <- AddRainData[AddRainData$Date >= startdate,]
  
  # rename to more readable column names
  AddRainData <- rename.Rain(AddRainData)
  AddRainData$Stations_id <- sprintf("%05i", type.convert(AddRainData$Stations_id, dec = ".", as.is="T"))
  
  # interpolated weather data in HUME format
  xportHUME <- InterpolateWeatherData(station_selected,
                                      RainStation_selected,
                                      weather_dat = weather_all, 
                                      RainData = AddRainData)
  
  
  xportHUME <- xportHUME %>% arrange(Time)
  
  
  #rm(xportw)
  return(xportHUME)
}




# Interpolation functions for DWD weather data ########################################




#' InterpolateWeatherData interpolates the weather data from the selected station to the location
#'
#' @param weather_dat weather data from the selected station in DWD format including the distance and
#' height difference to the location for which to interpolate the data
#' @param maxError maximum deviation for a parameter from the average of the station
#'  to be included in the interpolation
#'
#' @return interpolated data in the HUME formate
#' @export
#'
#' @examples
InterpolateWeatherData <- function(station_selected,
                                   RainStation_selected=NULL,
                                   weather_dat, 
                                   RainData=NULL,
                                   maxError=2,
                                   startdate="1990-01-01") {
  
  
  # Add the additional rain data to the data frame
  SelNames <- names(weather_dat)
  ## add RainData do weather data
  tmp <- bind_rows(weather_dat, RainData)
  weather_dat <- tmp[, ..SelNames]
  rm(tmp)
  
  
  # if additional stations are given, add to the selected stations
  if (!is.null(RainStation_selected)){
    stations_selected <- rbind(station_selected[,names(RainStation_selected)], RainStation_selected)
  }
  
  ## All data from selected weather station into long format
  # melt version
  wetter_long <- reshape2::melt(data = weather_dat, value.name = "Value", measure.vars=measvars, variable.name="variable" )
  # dplyr version
  #weather_dat <- as.data.frame(weather_dat)
  #wetter_long <- weather_dat %>% pivot_longer(cols = all_of(measvars), values_to = "Value", names_to = "variable")
  # set to data table format
  setDT(wetter_long)
  wetter_long$Jahr <- year(wetter_long$Date)
  wetter_long$Monat <- month(wetter_long$Date)
  
  # add column with mean values for each variable and day
  
  # data table version
  DailyMeanRegionalWeather_l <- wetter_long[ ,list(meanRegionValue=mean(Value, na.rm=T), sdRegionalValue = sd(Value, na.rm=T)),
                                             by=c("Date", "variable")]
  
  # dplyr version
  #  DailyMeanRegionalWeather_l <- wetter_long %>% group_by(Date, variable) %>% summarise(meanRegionValue=mean(Value, na.rm=T),
  #                                                                                       sdRegionalValue = sd(Value, na.rm=T))
  
  # data table version  
  MonthlyMeanRegionalWeather_l <- wetter_long[, .(MonthmeanRegionValue=mean(Value, na.rm=T)), by=c("Jahr", "Monat", "variable")]
  
  # dplyr version  
  #  MonthlyMeanRegionalWeather_l <- wetter_long %>% mutate(Jahr=year(Date), Monat=month(Date)) %>% 
  #    group_by(Jahr, Monat, variable) %>% 
  #    summarise(MonthmeanRegionValue=mean(Value, na.rm=T))
  
  
  
  # Hinzufügen von regionalem Mittelwert und Standardabweichung als neue Spalten in long-weather
  Weatherl <- merge(x = wetter_long, y = DailyMeanRegionalWeather_l, by=c("variable", "Date"))
  
  # calculate relative differences
  Weatherl <- Weatherl %>% mutate (RelDiff = (Value-meanRegionValue)/sdRegionalValue)
  
  # filter out values with too high differences
  Weatherl <- Weatherl %>% mutate(checkedValue = ifelse(abs(RelDiff)> maxError, NA, Value))
  
  # new code has been added at the end of the function
  # add monthly mean values to long weather data
#  Weatherl <- Weatherl %>% left_join(x=Weatherl, y=MonthlyMeanRegionalWeather_l, by=c("variable", "Jahr", "Monat"))
  
  # replace missing values with mean regional values 
#  Weatherl$Value <- ifelse(is.na(Weatherl$checkedValue), Weatherl$MonthmeanRegionValue, Weatherl$Value)
  
  # rain data are not filtered ...
  Weatherl[Weatherl$variable!="NIEDERSCHLAGSHOEHE","Value"] <- Weatherl[Weatherl$variable!="NIEDERSCHLAGSHOEHE","checkedValue"]
  #summary(Weatherl$Value)
  
  # delete columns with mean values, etc.
  wetter_long <- Weatherl %>% dplyr::select(-RelDiff, -meanRegionValue, -sdRegionalValue, -checkedValue)
  
  # merge weather data with station data
#  wetter_long <-  merge(x = wetter_long, y = station_selected[,c("Stationsname", "Distance_m")], by="Stationsname", all.x=T)
   wetter_long <-  left_join(x = wetter_long, y = station_selected[,c("Stations_id", "Distance_m")], by="Stations_id", all.x=T)
  
  # select rain observations of nearest weather station
  Raindata <- wetter_long %>% filter(variable == "NIEDERSCHLAGSHOEHE", !is.na(Value)) %>% dplyr::select(Stationsname, Date, variable, Value, Distance_m) %>%
    group_by(Date, variable) %>% slice(which.min(Distance_m))
  
  ## retransform selected weather data to wide format
  #ww <- wl %>% select(-Distance_m) %>% pivot_wider(id_cols = Date, names_from = variable, values_from = Value )

  # add Exceltime for later use
  Raindata$ExcelTime <- as.integer(difftime(Raindata$Date, as.Date("1899-12-30")))
  
  # apply inverse distance weighted average to the obtain the local data except for rainfall
  xport <- wetter_long %>% dplyr::select(Stationsname, Date, Jahr, Monat, variable, Value, Distance_m) %>%
    filter (!is.na(Value)) %>%
    dplyr::group_by(Jahr, Monat, Date, variable) %>%
    dplyr::summarise(sumValue=sum(Value/Distance_m), sum_1_Dist=sum(1/Distance_m)) %>%
    mutate(Value=sumValue/sum_1_Dist) %>%
    ungroup %>% dplyr::select(-sum_1_Dist, -sumValue)
  
  # transform to wide format
  xportw <- xport %>% pivot_wider(names_from = "variable", values_from = "Value")
  
  # replace interpolated rain fall data with data from nearest station if not missing
  
  # merge rain data with weather data
  xportw <- merge(x = xportw, y = Raindata[, c("Date", "Value")])
  
# use rain data from nearest station if available
  xportw$NIEDERSCHLAGSHOEHE <- ifelse(!is.na(Raindata$Value), Raindata$Value, xportw$NIEDERSCHLAGSHOEHE)  
  
  # remove additional Value column
  xportw$Value <- NULL
  
  # select colums to be checked for na values
  selcols <- names(xportw)[(!names(xportw) %in% c("Date", "Jahr", "Monat"))]
  
  # fill missing values with mean values of the month
  xportw <- xportw %>% 
    group_by(Jahr, Monat) %>% 
    mutate_at(selcols, na.aggregate)  
  
  # add additional time indicator
  xportw$ExcelTime <- as.integer(difftime(xportw$Date, as.Date("1899-12-30"))) 
  xportHUME <- getHUMEWeather(xportw)
  
  return(xportHUME)
}


  
# Analyze/add additional weather parameters ###########################################
  
#' Title
#'
#' @param df data frame with weather data
#'
#' @return data frame with additional weather parameters 
#' CumRain, CumRad, TempSum, PT, cPT, Rn, ra, rc, pETP, cumETP, climWbal, cumWbal
#' @export
#'
#' @examples
AddWeatherParameters <- function(df){ 
  stopifnot("Time" %in% names(df))
  stopifnot("GlobRad" %in% names(df))
  stopifnot("TMPM" %in% names(df))
  stopifnot("Rain" %in% names(df))
  stopifnot("Wind" %in% names(df))
  stopifnot("VP" %in% names(df))
  stopifnot("Sat_def" %in% names(df))

  df$Datum <- as.Date(df$Time, origin = "1899-12-30")
  df$Date <- df$Datum
  df$Year <- year(df$Datum)
  df$Month <- month(df$Datum)
  df$DOY <- yday(df$Datum)
  df <- df %>% group_by(Year) %>% mutate(CumRain = cumsum(Rain))
  df <- df %>% group_by(Year) %>% mutate(CumRad = cumsum(GlobRad))
  df <- df %>% group_by(Year) %>% mutate(TempSum = cumsum(pmax(0, TMPM)))
  df$PT <- 0
  df <- df %>% group_by(Year) %>% mutate(PT = ifelse(TMPM>0 , GlobRad/TMPM, 0))
  df <- df %>% group_by(Year) %>% mutate(cPT = cumsum(PT))
  df <- df %>% mutate(Rn = pmax(0, 0.6494 * (Rad_Int) - 18.417))
  
  # calculate aerodynamic resistance using the Thom-Oliver method
  df <- df %>% mutate(ra = ra_f(wind_speed = Wind, crop_height = 0.15, measure_height = 10, f_ra_funct = "Thom-Oliver"))
  #   df <- df %>% mutate(ra = ra_f(wind_speed = Wind, crop_height = 0.15, measure_height = 10, f_ra_funct = "Thom-Oliver"))
  
  # calculate canopy resistance of a well watered closed crop canopy (rc0=50, LAI=4) 
  df <- df %>% mutate(rc = rc_f_vectorized(rc0 = 50, LAI = 4))
  df <- df %>%  mutate(pETP = Penman(Temp = TMPM, Sat_def = Sat_def, Net_beam = Rn,
                                                         delta=delta_f(sat_vap_press = VP, Temp = TMPM),
                                                         gamma = 1013*Psycro, l_h_v_water = l_h_v_water,
                                                         ra = ra, rc = rc))
  # 
  df <- df %>% group_by(Year) %>% mutate(cumETP = cumsum(pETP))
  df <- df %>% mutate(climWbal = Rain-pETP)
  df <- df %>% group_by(Year) %>% mutate(cumWbal = cumsum(climWbal))
  return(df)
}  
  
  
  
  
# Import functions with rendering ######################################################


#' render_Wetter_Analysis
#'
#' @param Standort name of the site for which weather data should be estimated
#' @param geoBreite latitude of the site
#' @param geoLaenge longitude of the site
#' @param Hoehe_m height above sea level of the site
#' @param radius radius from which DWD weather stations should be included
#' @param minstations minimum number of stations to be included
#' @param startdate start date (string, format "1990-01-01")
#' @param maxError maximum deviation (%) of the values of a single weather station from the average of all stations
#' @param set_title
#' @param out_file name of the output file
#'
#' @return none, a html file is generated
#' @export
#'
#' @examples
render_Wetter_Analysis = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius, minstations, startdate, maxError, set_title, out_file) {

  ScriptFN <- paste0(localpath, "sources/", "AnalyzeWeatherClimate.rmd")
  rmarkdown::render(input = ScriptFN,
      params = list(
      Standort = Standort,
      set_title=set_title
    ),
    output_file = out_file, output_format = "html_document" #,  envir = new.env()
  )
}

#' render_Wetter_Import
#'
#' @param Standort
#' @param geoBreite
#' @param geoLaenge
#' @param Hoehe_m
#' @param radius
#' @param minstations
#' @param startdate
#' @param maxError
#' @param set_title
#' @param out_file
#'
#' @return
#' @export
#'
#' @examples
#render_Wetter_Import = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
#                                minstations, startdate, maxError, set_title, out_file)
  
  render_Wetter_Import = function(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
                                  minstations, startyear, endyear, maxError, set_title)
    
  
  {

  ScriptFN <- paste0(localpath, "DWD_Wetter_Import_new2.rmd")
  rmarkdown::render(input = ScriptFN,
                    params = list(
                      Standort = Standort,
                      geoBreite = geoBreite,
                      geoLaenge = geoLaenge,
                      Hoehe_m = Hoehe_m,
                      radius = radius,
                      minstations = minstations,
                      startyear = startyear,
                      endyear = endyear,
                      maxError = maxError,
                      set_title=set_title
                    ),
                    output_file = out_file, output_format = "html_document" #,  envir = new.env()
  )
}




#' WriteHumeWeatherFile
#'
#' @param df data frame with weather data
#' @param fn file name for output
#'
#' @return
#' @export
#'
#' @examples
WriteHumeWeatherFile <- function(df, fn) {
  # Validate file path
#  if (!file.exists(fn)) {
#    stop("The specified file path does not exist.")
#  }
  if (!all(namesHUME %in% names(df))) {
    stop("Not all column names for HUME file in data frame.")
  }


  df <- df[,namesHUME]
  # Datei öffnen
  #    fileHUME <- file(fn, open="wt", encoding="latin1")
  fileHUME <- file(fn, open="wb", encoding="UTF8")
  # Namen schreiben
  write(namesHUME, file = fileHUME, ncolumns = length(unitsHUME), append = FALSE, sep=",")
  # Einheiten schreiben
  write(unitsHUME, file = fileHUME, ncolumns = length(unitsHUME), append = TRUE, sep=",")
  # Daten schreiben
  write.table(df, file=fileHUME, append=TRUE, quote=FALSE, sep=",",#sep="\t",
              eol="\n", dec=".", row.names=FALSE, col.names=FALSE)
  # Datei muss explizit geschlossen werden
  close(fileHUME)
}


# Outdated functions ##################################################################

# Load DWD data from stationlist 

#' Title
#'
#' @param station_selected 
#' @param Hist_fst_file 
#' @param update_local 
#' @param startyear 
#'
#' @return
#' @export
#'
#' @examples
LoadDWDDatafromStationlist_old <- function(station_selected, 
                                       Hist_fst_file,
                                       update_local=T,
                                       Startyear= 1990){
  
  #Max_Height_diff <- 100 # maximum height difference (m) between location and station to be accepted  
  
  #long<-10.61     # longitude of location for which weather data have to be generated
  #lat <- 52.73    # latitude ...
  #Hoehe_m <- 100  # height above sea level ...
  #local=T         # source of weather station information
  #minstations<-15 # minimum number of stations to be included in data generation
  #maxError<-10    #
  #radius<-50000   # initial radius for searching weather stations
  #startyear <- "1990-01-01" # oldest date for weather data to be included
  
  idvars <- c("Date","Stationsname","geoBreite", "geoLaenge", "Stationshoehe" )
  measvars <- c("LUFTTEMPERATUR","LUFTTEMPERATUR_MAXIMUM","LUFTTEMPERATUR_MINIMUM","NIEDERSCHLAGSHOEHE",
                "REL_FEUCHTE", "WINDGESCHWINDIGKEIT",  "EstGlobRad", "VP", "Sat_def", "Rad_Int")
  
  maxError <- 2
  
  startdate <- paste0(as.character(StartYear), "-01-01")
  endyear <- year(Sys.Date())
  
  
  if (update_local==F) {
    # use the recent DWD  weather data
    filelist <- sort(stri_split_lines1(getURL(DWD_ftp_recent, dirlistonly = TRUE)))
    ziplist <- grep(".zip", filelist, value=TRUE)
    
    stationlist <- station_selected %>% arrange(Stations_id, desc(bis_datum))
    station_list <- stationlist %>% distinct(Stations_id, .keep_all = T)
    
    # retrieve stationids
    Stationids <- station_list$Stations_id
    ## length of
    n_df <- length(Stationids)
    ## prepare list of data frames for weather data import
    df_list <- vector(mode = "list", n_df)
    
    ## loop over the stations and load the data of the stations from DWD ftp server 
    for (i in 1:length(Stationids)){
      #  i <- 18
      station <- Stationids[i]
      #  df_list[[i]] <- try(getsingleDWDWeather(station = station, ziplist = ziplist, repository = repository, local = F), silent = T)
      df_list[[i]] <- getsingleDWDWeather(station = station, ziplist = ziplist, repository = DWD_ftp_recent, local = F)
    }
    
    ## remove empty data frames (no valid names etc.)
    df_list <-   df_list[lapply(df_list, length) > 1]
    
    ## convert list of data frames in one
    df <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
    rm(df_list)
    
    # rename and format of imported data columns 
    names(df)[names(df)=="MESS_DATUM"] <- "Datum"
    names(df)[names(df)=="QN_4"] <- "QUALITAETS_NIVEAU"
    names(df)[names(df)=="TMK"] <- "LUFTTEMPERATUR"
    names(df)[names(df)=="PM"] <- "LUFTDRUCK_STATIONSHOEHE"
    names(df)[names(df)=="UPM"] <- "REL_FEUCHTE"
    names(df)[names(df)=="FM"] <- "WINDGESCHWINDIGKEIT"
    names(df)[names(df)=="TXK"] <- "LUFTTEMPERATUR_MAXIMUM"
    names(df)[names(df)=="TNK"] <- "LUFTTEMPERATUR_MINIMUM"
    names(df)[names(df)=="RSK"] <- "NIEDERSCHLAGSHOEHE"
    names(df)[names(df)=="SDK"] <- "SONNENSCHEINDAUER"
    names(df)[names(df)=="Stations_id"] <- "Stations_id"
    df$Date = ymd(df$Datum)
    
    ## retrieve stations_id
    df$Stations_id <- sprintf("%05i", type.convert(df$STATIONS_ID, dec = ".", as.is = T))
    #df$Stations_id <- gsub(" ", "0", df$Stations_id)
    df <- df %>% left_join(station_list, by = "Stations_id")
    df$Datum<-as.Date(as.character(df$Datum),format="%Y%m%d")
    
    df <- df %>%
      mutate(geoLaenge = unlist(map(df$geometry,1)),
             geoBreite = unlist(map(df$geometry,2)))
    df <- df %>% dplyr::select(-geometry)
    
    # add estimates for radiation 
    df <- getRadWeather(df)
    
    tmp <- weather_historic  %>% filter(Stations_id %in% station_selected$Stations_id)
    tmp$Datum <-  as.Date(as.character(tmp$MESS_DATUM),format="%Y%m%d") 
    tmp$MESS_DATUM <- NULL
    selcols <- names(tmp)
    df <- df %>% dplyr::select(selcols) 
    df <- df %>% filter(Date > min(tmp$Date))
    wetter_dat <- rbind(tmp, df)
    rm(tmp)
  } else { 
    # use the local copy of DWD  weather data in fst format stored in weather_recent
    weather_all <- rbind(weather_historic, weather_recent)
    wetter_dat <- weather_all %>% filter(Stations_id %in% station_selected$Stations_id)
  }  
  
  ## add additional time indicators, remove Values older than start year
  wetter_dat <- wetter_dat %>% mutate(Jahr=year(Datum), Monat=month(Datum)) %>%
    dplyr::select(c(all_of(idvars),"Datum", "Jahr","Monat","ExcelTime",all_of(measvars))) 
  #rm(wetter_dat)
  
  ## All data from selected weather station into long format
  wetter_long <- reshape2::melt(data = wetter_dat, value.name = "Value", measure.vars=measvars, variable.name="variable" )
  
  #wetter_long <- wetter_dat %>% pivot_longer(cols = all_of(measvars), values_to = "Value", names_to = "variable")
  # set to data table format
  setDT(wetter_long)
  
  # add column with mean values for each variable and day
  mean_weatherl <- wetter_long[ ,list(meanValue=mean(Value)), by=c("Date", "variable")]
  #mean_weatherl <- wetter_long %>% group_by(Date, variable) %>% summarise(meanValue=mean(Value, na.rm=T))
  
  # Hinzufügen von Mittelwert als neue Spalte in long-weather (Version mit dplyr)
  Weatherl <- left_join(x = wetter_long, y = mean_weatherl, by=c("variable", "Date")) %>% filter(!is.na(Value))# %>% mutate(diff=Value-meanValue)
  #
  # calculate monthly averages and differences and relative between station data average values
  Weatherlmonth <- Weatherl %>% mutate(Jahr=year(Date), Monat=month(Date)) %>% group_by(Stationsname, variable, Jahr, Monat) %>%
    dplyr::summarise(mdiff=mean(Value-meanValue, na.rm=T), relmeandiff = mean(Value-meanValue, na.rm = T)/mean(meanValue, na.rm=T)*100)
  
  ## calculate relative differences   
  df_diff <- Weatherlmonth %>% group_by(Stationsname, variable) %>% dplyr::summarise(Abweichung=mean(relmeandiff)) %>% filter(!is.nan(Abweichung))
  
  ## select data with deviations from mean values of selected station above critical threshold 
  df_remove <- df_diff %>% filter(abs(Abweichung)>maxError) %>% mutate(ID=paste(Stationsname, variable, sep="_"))
  
  # remove observations with too high differences
  wetter_long <- wetter_long %>% mutate(ID = paste(Stationsname, variable, sep="_")) %>% filter(!(ID %in% df_remove$ID)) %>% dplyr::select(-ID)
  
  station_selected$Station <- station_selected$Stationsname
  wetter_long$variable <- as.character(wetter_long$variable)
  
  
  ## merge weather data with station data
  
  setkey(wetter_long, Stationsname)
  dt2 <- setDT(station_selected[,c("Stationsname", "Distance_m")])
  setkey(dt2, Stationsname)
  
  wetter_long <- dt2[wetter_long, on="Stationsname"]
  
  
  #  wetter_long <- merge.data.table(wetter_long, as.data.frame(station_selected[,c("Stationsname", "Distance_m")]), by.x=”Stationsname″, by.y=”Stationsname″)  
  #  wetter_long <-  merge(x = as.data.frame(wetter_long), y = as.data.frame(station_selected[,c("Stationsname", "Distance_m")]), by="Stationsname", all.x=T)
  
  ## select rain observations of nearest weather station
  Raindata <- wetter_long %>% filter(variable == "NIEDERSCHLAGSHOEHE", !is.na(Value)) %>% dplyr::select(Stationsname, Date, variable, Value, Distance_m) %>%
    group_by(Date, variable) %>% slice(which.min(Distance_m))
  
  ## retransform selected weather data to wide format
  #ww <- wl %>% select(-Distance_m) %>% pivot_wider(id_cols = Date, names_from = variable, values_from = Value )
  ## add Exceltime for later use
  Raindata$ExcelTime <- as.integer(difftime(Raindata$Date, as.Date("1899-12-30")))
  
  
  xport <- wetter_long %>% dplyr::select(Stationsname, Date, variable, Value, Distance_m) %>% filter (!is.na(Value)) %>%
    dplyr::group_by(Date, variable) %>% dplyr::summarise(sumValue=sum(Value/Distance_m), sum_1_Dist=sum(1/Distance_m)) %>% mutate(Value=sumValue/sum_1_Dist) %>% ungroup %>% dplyr::select(-sum_1_Dist, -sumValue)
  
  xportw <- xport %>% pivot_wider(names_from = "variable", values_from = "Value")
  
  xportw$NIEDERSCHLAGSHOEHE <- Raindata$Value
  
  xport[!complete.cases(xport),]
  
  xportw$ExcelTime <- as.integer(difftime(xportw$Date, as.Date("1899-12-30")))
  
  xportHUME <- getHUMEWeather(xportw)
  
  #rm(xportw)
  return(xportHUME)
}




#' GetWeatherData_selection_old gets the weather data for the selected stations
#'
#' @param stations_selected selected stations from DWD
#' @param DWD_content list structure of available DWD stations
#' @param local option for using local stored DWD zip-Files
#' @param startdate start date for selection of data
#'
#' @return
#' @export
#'
#' @examples
GetWeatherData_selection_old <- function(stations_selected,  DWD_content, local=T, startdate) {
  
  
  n_df <- length(stations_selected$Stations_id)
  ## prepare list of data frames for weather data import
  df_list <- vector(mode = "list", n_df)
  
  weather_all <- NULL
  
  # 
  for (station in stations_selected$Stations_id) {
    #    ID <- station_selected$Stations_id[1]
    if (local==TRUE){
      weather <- getcombinedDWDWeather(DWD_content = DWD_content, station = station, local = local,
                                       recent_repository = LocalCopy_DWD_ftp_recent,
                                       historical_repository = LocalCopy_DWD_ftp_historical )} else {
                                         weather <- getcombinedDWDWeather(DWD_content = DWD_content, station = station, local = local,
                                                                          recent_repository = DWD_ftp_recent,
                                                                          historical_repository = DWD_ftp_historical )}
    weather$Stations_id <- station
    df_list[[station]] <- weather
    
  }
  ## remove empty data frames (no valid names etc.)
  df_list <-   df_list[lapply(df_list, length) > 1]
  ## convert list of data frames in one
  
  weather_all <- try(data.table::rbindlist(df_list, use.names = T), silent = T)
  rm(df_list)
  weather_all <- as.data.frame(weather_all)
  weather_all <- weather_all %>% filter(Date >= as.Date(startdate))
  weather_all <- rename.weather(weather_all)
  weather_all <- getRadWeather(weather_all)
  weather_all$WINDGESCHWINDIGKEIT <-  windheight( ui = weather_all$WINDGESCHWINDIGKEIT,
                                                  zi =  weather_all$MHoeheWind, zo = 10)
  weather_all <- merge(x = weather_all, y = stations_selected[,c("Stations_id", "Distance_m", "Distance_km", "Height.Distance_m")], by="Stations_id")
  ## add additional time indicators, remove Values older than start date
  weather_all <- weather_all %>% mutate(Jahr=year(Date), Monat=month(Date),
                                        ExcelTime = as.integer(difftime(Date, as.Date("1899-12-30")))
  ) %>%
    dplyr::select(c(all_of(idvars), "Date", "Jahr","Monat","ExcelTime", all_of(measvars)))
  return(weather_all)
}


