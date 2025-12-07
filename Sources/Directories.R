library(here)

# lokale Data subdirectories
## mainpath
#localpath <- "./"
localpath <- here()


# DWD-Verzeichnisse für tägliche Wetterdaten allgemein
DWD_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
DWD_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/"

# DWD-Verzeichnisse für tägliche Niederschlagsdaten
DWDRain_ftp_historical <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/historical/"
DWDRain_ftp_recent <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/more_precip/recent/"

## outputpath
datapath <- here("data")
figurepath <- here("figures")


# DWD-Verzeichnisse für tägliche Wetterdaten allgemein
LocalCopy_DWD_ftp_historical <- here("LocalCopyDWD","kl","historical")
LocalCopy_DWD_ftp_recent <- here("LocalCopyDWD","kl","recent")


# DWD-Verzeichnisse für tägliche Niederschlagsdaten
LocalCopy_DWD_Rain_ftp_historical <- here("LocalCopyDWD", "more_precip","historical")
LocalCopy_DWD_Rain_ftp_recent <- here("LocalCopyDWD","more_precip","recent")


## local DWD Rdata paths
Local_DWD <- here("LocalCopyDWD")
Local_R_DWD <- here("LocalCopyDWD","Rdata")
Local_DWD_historical <- here("historical")
Local_DWD_recent <- here("recent")

Local_Rain_DWD_historical <- here("historical/")
Local_Rain_DWD_recent <- here("recent/")


## local RData file names

fn_DWD_content <- "DWD_content.Rdata"
fn_DWDRain_content_recent <- "DWDRain_content_recent.Rdata" 

fn_histDWD_data <- "weather_dat_1990.fst"


fn_HistoricalDWD <- "historicalDWDweather.RData"
fn_HistoricalDWDRain <- "historicalDWDRain.RData"
