# this file contains some code initialize variables for the Shiny DWD weather app
  
# libraries ############################################################
library(dplyr)
library(sf)
library(raster)
library(fst)
library(lubridate)
library(data.table)
library(scales)
#
#localpath <- paste0(here(),"/")

# source functions ####################################################
#source(paste0(localpath, "Sources/Directories.R"))
#source(paste0(localpath, "Sources/EvapFuncs.R"))
#source(paste0(localpath, "Sources/WeatherFunctions.R"))

#detach("package:here", unload=TRUE)
setwd(getwd())
library(here)

fn_DWD_content <- "DWD_content.Rdata"
fn_DWDRain_content <- "DWDRainStationlist.RData" 
fn_histDWD_data <- "weather_dat_1990.fst"


# height model for Germany ############################################
Hmodell <- raster( here("dgm200.utm32s.gridascii","dgm200","dgm200_utm32s.asc"))

# load station metadata from local file ################################
fn <- here("LocalCopyDWD","Rdata", fn_DWD_content) #"LocalCopyDWD/Rdata/DWDstationlist.Rdata")
load(file = fn)


# load recent weather meta data from local fst file ##########################
fn <- here("LocalCopyDWD","Rdata", fn_DWDRain_content) #"LocalCopyDWD/Rdata/DWDstationlist.Rdata")
load(file = fn)

# load meta data for rain stations
#DWDRain_content <- getDWDRainStationList(recent = DWDRain_ftp_recent, historical = DWDRain_ftp_historical)
#save(DWDRain_content, file = fn)
fn <- here("LocalCopyDWD","Rdata", fn_DWDRain_content)
load(file = fn)



station_list <- DWD_content$stations %>% mutate (labs = paste0( "<p>", Stationsname, "<p></p>", 
                                                        "Höhe: ", as.character(Stationshoehe), "</p><p>",
                                                        "Daten bis: ", as.character(bisJahr), "</p><p>")) 

station_list <- st_as_sf(x = station_list, coords = c("geoLaenge", "geoBreite")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")
  
DWD_Stations <- st_as_sf(x = station_list, coords = c("geoLaenge", "geoBreite")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")
DWD_Stations$running <- DWD_Stations$bisJahr == year(Sys.Date())
DWD_Stations$fillcolour <- ifelse(DWD_Stations$running, "darkgreen", "red")
#SelDWD_Stations <- DWD_Stations


RainStation_list <- DWDRain_content$stations %>% mutate (labs = paste0( "<p>", Stationsname, "<p></p>", 
                                                                           "Höhe: ", as.character(Stationshoehe), "</p><p>",
                                                                           "Daten bis: ", as.character(bisJahr), "</p><p>")) 

RainStation_list <- st_as_sf(x = RainStation_list, coords = c("geoLaenge", "geoBreite")) %>%
  st_set_crs(value = "+proj=longlat +datum=WGS84")

#DWD_data <- NULL
Max_Height_diff <- 200
n_years <- 30

# set up year choices
actyear <- year(Sys.Date())
year_choices <- as.list(as.character(seq(1990, actyear, 1)))
names(year_choices) <- year_choices


# available columns after analysis
ColChoices <- c("TMPM","TempSum" , "TMPMN", "TMPMX","Rain", "CumRain", "LF", "Sat_def",  "GlobRad" ,"CumRad" ,
                "Wind",  "Rn", "pETP", "cumETP", "climWbal", "cumWbal")# "PT", "cPT",

# load historic weather data from local fst file ####
fn <- here("LocalCopyDWD","Rdata", fn_histDWD_data)
weather_historic <- read.fst(path = fn)


# set to data table format to increase speed #####
weather_historic <- setDT(weather_historic)
## select historic data from choosen weather stations



