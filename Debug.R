rm(list = ls(all.names = TRUE))
library(Weatherfunctions)
library(sf)
library(dplyr)
source("global.R")



Loc <- data.frame(geoBreite=52.7, geoLaenge=10.4)
# create an sf object from Location data frame
Loc <- st_as_sf(Loc, coords = c("geoLaenge", "geoBreite")) %>% st_set_crs(value = "+proj=longlat +datum=WGS84")
# convert to ETRS89 / UTM zone 32N because altitude data are in this format
Loc_UTM <- st_transform(x = Loc, crs = 25832)
# retrive altitude data
ElevLoc <- raster::extract(x = Hmodell, y = Loc_UTM)

Loc$ASL <- ElevLoc

Loc <- st_as_sf(Loc, coords = c("geoLaenge", "geoBreite", "ASL"))%>% st_set_crs(value = "+proj=longlat +datum=WGS84")



DWD_Stations$Distance_m <- pmax(1,as.numeric(st_distance(DWD_Stations, Loc))) # minimum distance is 1 m because

SelStations <- DWD_Stations %>%
  mutate("Height.Distance_m" = Stationshoehe - Loc$ASL) %>%
  filter(bis_datum >= as.numeric(2024),
         abs(Height.Distance_m) < 100) %>%
  arrange(Distance_m) %>% slice_head(n=10)


RainStation_list$Distance_m <- pmax(1,as.numeric(st_distance(RainStation_list, Loc))) # minimum distance is 1 m because

SelDWDRainStations <- RainStation_list %>%
  mutate("Height.Distance_m" = Stationshoehe - Loc$ASL) %>%
  filter(bis_datum >= as.numeric(2024),
         abs(Height.Distance_m) < 100) %>%
  arrange(Distance_m) %>% slice_head(n=1)



DWD_data <- LoadDWDDatafromStationlist(station_selected = SelStations,
                                            RainStation_selected = SelDWDRainStations,
                                            DWD_content = DWD_content,
                                            DWDRain_content = DWDRain_content,
                                            weather_historic = weather_historic,
                                            local = F,
                                            StartYear = 1993)
