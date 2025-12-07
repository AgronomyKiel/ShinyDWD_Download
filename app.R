
# rm(list = ls(all.names = TRUE))
library(tidyverse)
library(gridlayout)
library(shiny)
library(shinyFiles)
library(ggplot2)
library(leaflet)
library(leaflet.extras2)
library(leaflegend)
library(leafem)
library(sf)
library(sp)
library(data.table)
library(lubridate)
#library(xlsx)
library(writexl)
library(htmltools)
library(raster)
library(shinybusy)
library(shinythemes)
library(markdown)
library(DT)
library(Weatherfunctions)

# global variables
source('global.R')

##### User interface #####

ui <- navbarPage(
  title = "Wetterdatenimport DWD",
  header = tagList(
    add_busy_spinner(spin = "fading-circle", color = "#FFF", margins = c(0, 10))
  ),
  collapsible = FALSE,
  #theme = bslib::bs_theme(),
  theme = shinytheme(theme = "superhero"),
  id = "tabset-default-id",
  tags$head(tags$script(src = "message-handler.js"),
            tags$style(HTML(
              "
                  .dataTables_length label,
                  .dataTables_filter label,
                  .dataTables_info {
                      color: white!important;
                      }

                  .paginate_button {
                      background: white!important;
                  }

                  thead {
                      color: white;
                      }

                  "))),
  selected = "Info",
  tabPanel(title = "Info", 
#           includeHTML("ShinyDWDDisclaimer.html")),
           includeMarkdown("ShinyDWDDisclaimer.md")),
tabPanel(
    ###### Panel Standort/Jahre #######
    title = "Standort/Jahre",
    grid_container(
      layout = c(
        "GeoDaten Karte",
        "Zeit     Karte"),
      row_sizes = c(
        "0.6fr",
        "0.8fr"
      ),
      col_sizes = c(
        "0.5fr",
        "1.5fr"),
      gap_size = "8px",
      grid_card(
        area = "GeoDaten",
        title = "Koordinaten",
        textInput(
          inputId = "Lat",
          label = "Breitengrad",
          value = "52.7306"),
        textInput(
          inputId = "Lon",
          label = "Längengrad",
          value = "10.6298"),
        textInput(
          inputId = "ASL",
          label = "Höhe NN",
          value = "85")
      ),
      grid_card(
        area = "Zeit",
        title = "Zeit",
        selectInput(
          inputId = "mySelectStartYear",
          label = "Beginne mit Jahr",
          choices = year_choices,
          selected = as.character(as.integer(format(Sys.Date(), "%Y"))-n_years)
        ),
        selectInput(
          inputId = "mySelectEndYear",
          label = "Ende mit Jahr",
          selected = as.integer(format(Sys.Date(), "%Y")),
          choices = seq(as.integer(format(Sys.Date(), "%Y"))-n_years, as.integer(format(Sys.Date(), "%Y")),1)
        ),
        actionButton(inputId = "BtnLoadWeather", "Daten laden/interpolieren")
        
      ),
      grid_card(
        area = "Karte",
        title = "Karte",
        leafletOutput("mymap",
                      height = 1000
        )
      )
    )
  ),
  

###### Panel Wetterdaten #######
# the panel consists of two cards, 
# the left card contains the action buttons and 
# the right card contains the table with the weather data  
  tabPanel(title = "Wetterdaten",
           grid_container(
             layout = c(
               "Button   Daten"),
             row_sizes = c(
               "2fr"
             ),
             col_sizes = c(
               "0.3fr",
               "1.7fr"),
             gap_size = "10px",
             
             # grid card with action buttons
             grid_card(
               area = "Button",
               title = "Aktion",
               fileInput(inputId = "LoadSavedFile", "Select a file"),
               textOutput("FirstWeatherDate"),
               textOutput("LastWeatherDate"),
#               dateInput(inputId = "LastWeatherDate", "Daten geladen bis:", value = max(weather_all$Datum)),
#               actionButton(inputId = "BtnUpdateLocalWeather", "Lokale Daten aktualisieren"),
#               shinySaveButton(id = "save", "Save file", "Save file as ...", viewtype = "icon", filetype=list(csv="csv")
               downloadButton("downloadData", "Download")
             ),
# grid card with table of weather data
            grid_card(
              area = "Daten",
              title = "Daten",
              div(
                style = "display: flex; flex-direction: column; gap: 12px;",
                DT::dataTableOutput("TableWetterdaten", height = "420px"),
                tableOutput("TableWettersummary")
              ))
          )
  ),

###### Panel Wetterauswertung #######
# the panel consists of two cards, the left card contains the action buttons and the right card contains the plot with the weather data
  tabPanel(title = "Wetterauswertung",
           grid_container(
             layout = c(
               "ButtonAuswertung   Plot"),
             row_sizes = c("4fr"),
             col_sizes = c("0.3fr","1.7fr"),
             gap_size = "10px",
             grid_card(
               area = "ButtonAuswertung",
               title = "Aktion",
               actionButton(inputId = "AnalyzeWeather", "Daten auswerten"),
               #               actionButton("BtnSaveWeather", "Daten speichern")
               #shinySaveButton(id = "save", "Save file", "Save file as ...", viewtype = "icon",filetype=list(xlsx="xlsx"))
               #               downloadButton("downloadData", "Download", ... = )
               selectInput(
                 inputId = "mySelectParam",
                 label = "Parameter",
                 selected = ColChoices[1],
                 choices = ColChoices#          year_choices 
               ),
               selectInput(
                 inputId = "mySelectAnalysisYear",
                 label = "Analysejahr",
                 selected = as.integer(format(Sys.Date(), "%Y")),
                 choices = seq(as.integer(format(Sys.Date(), "%Y"))-n_years, as.integer(format(Sys.Date(), "%Y")),1)#          year_choices 
               )
             ),
            grid_card(
              area = "Plot",
              title = "Auswertung",
              div(
                style = "height: 80vh;",
                plotOutput("plot", click = "plot_click", height = "100%")
              ))
          )
  )
)

##### Server logic #####

server <- function(input, output, session) {
  updateTabsetPanel(session = session, "Wetterdatenimport DWD",
                    selected = "Info")
  
  ##### output Map ######
  
  output$mymap <- renderLeaflet({
    loc <- Location()
    sf::st_crs(loc) <- "+proj=longlat +datum=WGS84"
    DWD_Stations$Running <- as.factor(DWD_Stations$running)
    factorPal <- colorFactor(palette = 'Set1', DWD_Stations$Running)
    
    m <- leaflet(data = DWD_Stations, options = leafletOptions(
      contextmenu = TRUE,
      contextmenuWidth = 200,
      contextmenuItems =
        context_mapmenuItems(
          context_menuItem("Zoom Out", "function(e) {this.zoomOut()}", disabled=FALSE),
          "-",
          context_menuItem("Zoom In", "function(e) {this.zoomIn()}"))))  %>%
      addTiles(group = "base") %>%
      setView(st_coordinates(loc)[1], st_coordinates(loc)[2], zoom = 10) %>%
#      addCircles(data = station_list, radius=65, color="darkred", group = "DWD station") %>% 
      addCircles(data = RainStation_list, radius=65, color="darkblue", group = "Rainstation only") %>% 
      ## add circle for selected location
#      addCircles(st_coordinates(loc)[1], st_coordinates(loc)[2], radius=50, color="darkred", group = "SelLocation") %>% 
      addMarkers(st_coordinates(loc)[1], st_coordinates(loc)[2]) %>% 
      addContextmenu() %>%
      leafem::addMouseCoordinates(m) %>%
      addCircleMarkers(popup = ~paste(Stationsname, "<br>Höhe: ", Stationshoehe,
                                      "<br>Daten bis:", bisJahr,
                                      "<br>Daten von:", vonJahr), 
                       color = ~ factorPal(Running),
                       #fillColor = ~fillcolour, 
                       radius = 7, 
                       #color = ~fillcolour
                       opacity = 0.8,
                       fillOpacity = 0.8
                       ) %>%
      addLegendFactor(
        pal = factorPal,
        title = htmltools::tags$div('Active', 
        style = 'font-size: 14px; color: darkgreen;'),
        values = DWD_Stations$Running,
        position = 'topright',
        shape = 'circle',
        width = 20,
        height = 20
      ) %>% 

    addLayersControl(baseGroups = "base",
                     # overlayGroups = c("wmsgroup"))
                     overlayGroups = c("L849"))
  })
  
  #### output table Wetterdaten #####
  output$TableWetterdaten <- renderDataTable({
     data <- DWD_data$data %>% top_n(n = 15, wt = Time) %>% arrange(Time)
     data$Datum <- as.character(as.Date(data$Time, origin = "1899-12-30"))
     data$Time <- NULL
     data <- data %>%
       dplyr::select("Datum", everything()) 
#     data <- validate(
#       need(nrow(data) > 0, "No Data to show")
#     )
     data <- datatable(data) %>% formatStyle(columns=names(data), color="white")
     data
  } )
  
  #### output table Wettersummary #####
  output$TableWettersummary <- renderTable({
    df <- DWD_data$data
    df$Time <- NULL
    df <- as.data.frame(do.call(cbind, lapply(df, summary, digits=3)))
    df$stat <- rownames(df)
    rownames(df) <- NULL
    df <- df %>% dplyr::select(stat, everything())
    df
  }, caption="Summary of weather data",caption.placement = "top" )
  
    
#local_data <- reactiveValues()

#### output plot weather analysis #####
  output$plot <- renderPlot({
#    write.csv(DWD_data$data, file="DWD_data.csv")
    df <- AddWeatherParameters(DWD_data$data)
#    write.csv( df , file="DWD_data2.csv")
    makeplot(df = df, parameter = input$mySelectParam, BaseSize = 16, 
                             SelYear = input$mySelectAnalysisYear, ShiftYears=T)
#    write.csv( df, file = "DWD_data3.csv")
  }, res = 96)
  
#### output last weather date #####
output$LastWeatherDate <- renderText({LastDateWeather()})

output$FirstWeatherDate <- renderText({FirstDateWeather()})


LastDateWeather <- reactive({
    if (!is.null(DWD_data$data)){
  paste0("Wetter bis: ", as.character(as.Date(max(DWD_data$data$Time),  origin = "1899-12-30")))} else {
    "Wetter bis: "
  }
  
})
  
FirstDateWeather <- reactive({
  if (!is.null(DWD_data$data)){
    paste0("Wetter von: ", as.character(as.Date(min(DWD_data$data$Time),  origin = "1899-12-30")))} else {
      "Wetter bis: "
    }
  
})



  
# reactive selected location for weather data estimation ######
  
  Location <- reactive({
    click <- input$mymap_click
    if  (!is.null(click)){  # clicked on the map, take location from click location
      Loc <- data.frame(geoBreite=round(click$lat, 6), geoLaenge=round(click$lng, 6))
      # create an sf object from Location data frame
      Loc <- st_as_sf(Loc, coords = c("geoLaenge", "geoBreite")) %>% st_set_crs(value = "+proj=longlat +datum=WGS84")
      # convert to ETRS89 / UTM zone 32N because altitude data are in this format
      Loc_UTM <- st_transform(x = Loc, crs = 25832)
      # retrive altitude data
      ElevLoc <- raster::extract(x = Hmodell, y = Loc_UTM)
      Loc <- data.frame(geoBreite=round(as.numeric(click$lat), 6), geoLaenge=round(as.numeric(click$lng), 6), ASL=ElevLoc)
      st_as_sf(Loc, coords = c("geoLaenge", "geoBreite", "ASL"))%>% st_set_crs(value = "+proj=longlat +datum=WGS84")}
    else{ # not yet clicked on the map, take location from default of inputs
      Loc <- data.frame(geoBreite = round(as.numeric(input$Lat), 6), geoLaenge=round(as.numeric(input$Lon), 6))
      # create an sf object from Loc data frame
      Loc <- st_as_sf(Loc, coords = c("geoLaenge", "geoBreite")) %>% st_set_crs(value = "+proj=longlat +datum=WGS84")
      # convert to ETRS89 / UTM zone 32N because altitude data are in this format
      Loc_UTM <- st_transform(x = Loc, crs = 25832)
      # retrive altitude data
      ElevLoc <- raster::extract(x = Hmodell, y = Loc_UTM)
      # create a data frame with location and altitude
      Loc <- data.frame(geoBreite=round(as.numeric(input$Lat), 6), geoLaenge=round(as.numeric(input$Lon), 6), ASL=ElevLoc)
      # create an sf object from Loc data frame
      st_as_sf(Loc, coords = c("geoLaenge", "geoBreite", "ASL")) %>% st_set_crs(value = "+proj=longlat +datum=WGS84")

    }})


  #### observe event map is clicked ####
  observeEvent(input$mymap_click, {
    click <- input$mymap_click
    loc <- Location()
    ## set crs of loc to WGS84
    sf::st_crs(loc) <- "+proj=longlat +datum=WGS84"
    updateTextInput(inputId = "Lon", value = paste(round(click$lng,4)))
    updateTextInput(inputId = "Lat", value = paste(round(click$lat,4)))
    updateTextInput(inputId = "ASL", value = paste(round(st_coordinates(loc)[3],4)))

    ## create a proxy for the map
    proxy <- leafletProxy("mymap")
    ## This displays the pin drop circle
    proxy %>%
      # clear the group new_point
      clearGroup(group = "new_point") %>%

      # add circle for selected location
      addCircles(as.numeric(click$lng), as.numeric(click$lat), radius=50, color="darkred", group = "new_point")
       proxy %>% addCircles(as.numeric(click$lng), as.numeric(click$lat), 
                            radius=ActRadius(), color="red", group = "new_point", opacity = 0.3) #%>%
    # add color mark for selected DWD stations
       proxy %>% addCircles(data=SelDWD_Stations(), radius=80, color="yellow", group = "new_point", opacity = 1) #%>%
    # add color mark for selected Rain stations   
    proxy %>% addCircles(data=SelDWDRain_Stations(), radius=160, color="blue", group = "new_point", opacity = 1) #%>%
    
    # if data is already loaded, remove it
    if (!is.null(DWD_data$data)) {
      DWD_data$data <- NULL
    } 
    
  })



  #### interpolated/selected data from DWD stations as reactive data ####

DWD_data <- reactiveValues()

observeEvent(input$BtnLoadWeather,{
  loc <- Location()
  coords <- st_coordinates( loc)
  
  geoLaenge <- coords[, "X"]
  geoBreite <- coords[, "Y"]
  Hoehe_m <- coords[, "Z"]
  
  
  # retrieve the recent station list from the DWD content
  stationlist <- DWD_content$historical$stationlist
  
  # make an sf object from stationlist data frame
  stationlist <- st_as_sf(stationlist, coords = c("geoLaenge", "geoBreite")) %>%
    st_set_crs(value = "+proj=longlat +datum=WGS84")
  
  # add the distance to the location to the stationlist data frame
  stationlist$Distance_m <- pmax(1,as.numeric(st_distance(stationlist, loc))) # minimum distance is 1 m because
  stationlist$Distance_km <- as.numeric(format(stationlist$Distance_m /1000, digits = 3))
  # remove the geometry column
  stationlist$geometry <- NULL
  stationlist <- as.data.frame(stationlist)
  
  
  DWD_data$data <- NULL
  
  fn_histDWD_data <- paste0(fn_histDWD_data_str, as.character(1990),".fst")
  
#  fn <- here(Local_R_DWD, fn_histDWD_data)
  fn <- here("LocalCopyDWD","Rdata", fn_histDWD_data)
  
  weather_historic <- read.fst(path = fn)
  
  # select the columns that are in the core, i.e. essential data set and rename them
  weather_historic_core <- weather_historic[,c("Stations_id", "Date",longNames_DWD_core)]
  rm(weather_historic)
  
  startdate <- paste0(as.character(input$mySelectStartYear), "-01-01")
  RainStationList <- DWDRain_content$historical$stationlist
  # select the nearest stations with rain data, for historical data it is necessary to select more than one station
  # because stations with rain data changed over time
  RainStations_selected <- SelectStations (lat=geoBreite,
                                           long=geoLaenge,
                                           height_loc=Hoehe_m, 
                                           stationlist = RainStationList,
                                           #DWDRain_content$recent$stationlist,
                                           minstations=3,
                                           max_stations = 3,
                                           radius=80000,
                                           startdate=startdate,
                                           max.Height.Distance_m=150)
  
  
  # get the rain data for the selected stations
  
  df_Rain <- GetRainData_selection(RainStations_selected,
                                   DWDRain_content,#$historical,
                                   repository=DWDRain_ftp_historical,
                                   startdate="1990-01-01") 
  
  
  # add the distance to the rain station to the data frame
  
  df_Rain <- left_join(df_Rain, RainStations_selected[,c("Stations_id", "Distance_km")], by="Stations_id")
  
  # interpolate the weather data for the location
  IntPolDWDHistorical <- InterpolateFromDWD(df_DWD_core = weather_historic_core, stationlist = stationlist, 
                                            geoBreite = geoBreite, geoLaenge = geoLaenge, 
                                            max.Height.Distance_m=150, Hoehe_m = Hoehe_m, df_Rain = df_Rain, 
                                            startdate = startdate) 
  IntPolDWDdataHistorical <- IntPolDWDHistorical$DWDdata
  

### load an interpolate the recent weather data  
  fn_recentDWD_data_str <- "recent_weather_dat_"  
  fn_recentDWD_data <- paste0(fn_recentDWD_data_str, as.character(1990),".fst")
  
  #  fn <- here(Local_R_DWD, fn_histDWD_data)
  fn <- here("LocalCopyDWD","Rdata", fn_recentDWD_data)

  weather_recent <- read.fst(path = fn)
  
  # select the columns that are in the core, i.e. essential data set and rename them
  weather_recent_core <- weather_recent[,c("Stations_id", "Date",longNames_DWD_core)]
  rm(weather_recent)
  startdate <-  as.Date(max( IntPolDWDdataHistorical$Time), origin = "1899-12-30") + 1
  
  RainStationList <- DWDRain_content$recent$stationlist
  # select the nearest stations with rain data, for historical data it is necessary to select more than one station
  # because stations with rain data changed over time
  RainStations_selected <- SelectStations (lat=geoBreite,
                                           long=geoLaenge,
                                           height_loc=Hoehe_m, 
                                           stationlist = RainStationList,
                                           #DWDRain_content$recent$stationlist,
                                           minstations=3,
                                           max_stations = 3,
                                           radius=80000,
                                           startdate=startdate,
                                           max.Height.Distance_m=150)
  
  
  # get the rain data for the selected stations
  
  df_Rain <- GetRainData_selection(RainStations_selected,
                                   DWDRain_content,#$historical,
                                   repository=DWDRain_ftp_recent,
                                   startdate=startdate) 
  
  
  # add the distance to the rain station to the data frame
  
  df_Rain <- left_join(df_Rain, RainStations_selected[,c("Stations_id", "Distance_km")], by="Stations_id")
  
  # interpolate the weather data for the location
  IntPolDWDrecent <- InterpolateFromDWD(df_DWD_core = weather_recent_core, stationlist = stationlist, 
                                            geoBreite = geoBreite, geoLaenge = geoLaenge, 
                                            max.Height.Distance_m=150, Hoehe_m = Hoehe_m, df_Rain = df_Rain, 
                                            startdate = startdate) 
  IntPolDWDdatarecent <- IntPolDWDrecent$DWDdata
  
  
  
  DWD_data$data <- rbind(IntPolDWDdataHistorical, IntPolDWDdatarecent)
  
  #   
#    input$LastWeatherDate <- Date
    updateTabsetPanel(session, "Wetterdatenimport DWD",
                      selected = "Wetterdaten")
  })

observeEvent(input$LoadSavedFile,{
  file <- input$LoadSavedFile
  req(file)
  ext <- tools::file_ext(file$datapath)
  validate(need(ext == "csv", "Please upload a csv file"))
  DWD_data$data <- read.csv(file$datapath, header = T)   
#  StartYear <- as.numeric(input$mySelectStartYear)
#  startyear <- paste0(as.character(StartYear), "-01-01")
  
})


#### observe event analyze weather button is clicked ####
  
  observeEvent(input$AnalyzeWeather,{
    DWD_local()
  })
  
#### download data ####
  
output$downloadData <- downloadHandler(
  filename = function() {
    paste("Weather_",input$Lat,"_",input$Lon, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(DWD_data$data, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)



#### reactive data selected year #####
  SelYear <- reactive({
    SelYear <- as.numeric(input$mySelectAnalysisYear)
  })
  
  
#### reactive data DWD_local #####
# contains additional derived data from the local interpolated DWD-data  
# q: how to check if DWD_data$data is not empty?



  DWD_local <- reactive({
#    req(length(DWD_data$data$Time) !=0)
    DWD_local <- AddWeatherParameters(DWD_data$data)
  })
}

shinyApp(ui, server)
