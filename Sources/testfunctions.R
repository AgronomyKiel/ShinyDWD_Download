
rm(list = ls())

# libraries ############################################################
library(dplyr)
library(sf)
library(raster)
library(here)
library(ggplot2)
library(microbenchmark)

#
localpath <- paste0(here(),"/")



# source functions ####################################################
source(here("Sources","Directories.R"))
source(here( "Sources","EvapFuncs.R"))
source(here( "Sources", "WeatherFunctions.R"))


fn <- here("data","Alldata.fst")

df <- read_fst(fn)


summary(df)



fn <- "C:/Users/h_kage/Downloads/Weather_52.7306_10.6298-2.csv"

df <- read.csv(file = fn, header = T) 

df$Datum <- as.Date(df$Time, origin = "1899-12-30")

summary(df$Datum)



tmp <- tibble::tibble(summary(df))


tmp <- as.data.frame(do.call(cbind, lapply(df, summary)))
tmp
tmp$stat <- rownames(tmp)
rownames(tmp) <- NULL
tmp <- tmp %>% select(stat, everything())
tmp


df <- df %>% mutate(Date = as.Date(Time, origin = "1899-12-30" ))

df <- df %>% mutate(Rn = pmax(0, 0.6494 * (Rad_Int) - 18.417))

LAI <- seq(0.1, 5, 0.1)
rc <- rc_f_vectorized(rc0 = 50, LAI = LAI)
rc2 <- Vectorize(rc_f)(rc0 = 50, LAI = LAI)

rc-rc2

illu <- data.frame(LAI, rc, rc2)

p <- ggplot(data=illu, aes(x=LAI, y=rc)) + geom_line() + geom_line(aes(x=LAI, y=rc2), color = "red")
p

microbenchmark(rc_f_vectorized(rc0 = 50, LAI = LAI), Vectorize(rc_f)(rc0 = 50, LAI = LAI))


df <- df %>% mutate(ra = ra_f(wind_speed = Wind, crop_height = 0.15, measure_height = 10, f_ra_funct = "Thom-Oliver"))

df <- df %>% mutate(rc = rc_f_vectorized(rc0 = 50, LAI = 4))


df <- df %>%  mutate(pETP = Penman(Temp = TMPM, Sat_def = Sat_def, Net_beam = Rn,
                                              delta=delta_f(sat_vap_press = VP, Temp = TMPM),
                                              gamma = 1013*Psycro, l_h_v_water = l_h_v_water,
                                              ra = ra, rc = rc))
summary(df$pETP)


df$Year <- as.numeric(format(df$Date, "%Y"))
df$DOY <- as.numeric(format(df$Date, "%j"))
df <- AddWeatherParameters(df)



makeplot(dfweather = df, parameter = "TMPM", BaseSize = 16, SelYear = 2024)



