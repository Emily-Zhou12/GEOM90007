library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(httr)
library(shinydashboard) 
library(shinyWidgets)
library(shinyjs)

source('tableau-in-shiny-v1.0.R')

###########
#Load Data#
###########
# Load the tram track data from the CSV file
tram_data <- read.csv("cleaned_data/tram-tracks.csv")
# Load the tram stop data from the CSV file
tram_stop_data <- read.csv("cleaned_data/city-circle-tram-stops.csv")
# Load the taxi rank data from the CSV file
taxi_data <- read.csv("cleaned_data/taxi-ranks.csv")
# Load the bus stop data from the CSV file
bus_data <- read.csv("cleaned_data/bus-stops.csv")

# Load the POI cleaned Data
restaurants_data <- read.csv("cleaned_data/cafes-and-restaurants.csv")
museums_data <- read.csv("cleaned_data/art-galleries-and-museums.csv")
churches_data <- read.csv("cleaned_data/churches.csv")
clubs_data <- read.csv("cleaned_data/clubs.csv")
theaters_data <- read.csv("cleaned_data/entertainment-and-theatre.csv")
memorials_data <- read.csv("cleaned_data/memorials-and-sculptures.csv")
parks_data <- read.csv("cleaned_data/parks-and-sports-facilities.csv")
toilets_data <- read.csv("cleaned_data/public-toilets.csv")
visitorCentres_data <- read.csv("cleaned_data/visitors-centres.csv")

# Adding source of POI data to each dataset
restaurants_data$source <- "cafes-and-restaurants.csv"
museums_data$source <- "art-galleries-and-museums.csv"
churches_data$source <- "churches.csv"
clubs_data$source <- "clubs.csv"
theaters_data$source <- "entertainment-and-theatre.csv"
memorials_data$source <- "memorials-and-sculptures.csv"
parks_data$source <- "parks-and-sports-facilities.csv"
toilets_data$source <- "public-toilets.csv"
visitorCentres_data$source <- "visitors-centres.csv"

#####################
#Data Pre-processing#
#####################
# filter the tram track data
tram_track_data <- tram_data$Geo.Shape

# filter the tram stops data (separate Latitude &Longitude)
tram_stop_data <- tram_stop_data %>%
  separate(Geo.Point, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))
# set the icon image for tram stops
tram_stop_icon <- makeIcon(
  iconUrl = "icons/tram.png",  
  iconWidth = 12,             
  iconHeight = 12              
)

# filter the taxi ranks data (separate Latitude &Longitude)
taxi_data <- taxi_data %>%
  separate(Geo.Point, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))
# set the icon image for taxi ranks
taxi_icon <- makeIcon(
  iconUrl = "icons/taxi.png",  
  iconWidth = 12,             
  iconHeight = 12             
)

# set the icon image for visitor center
visitorCentres_icon <- makeIcon(
  iconUrl = "icons/visitors-centres.png",
  iconWidth = 12, 
  iconHeight = 12
)

# filter the bus stop data (separate Latitude &Longitude)
bus_data <- bus_data %>%
  separate(Geo.Point, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))
# set the icon image for bus stops
bus_icon <- makeIcon(
  iconUrl = "icons/bus.png",  
  iconWidth = 12,             
  iconHeight = 12              
)

# obtain the real-time weather using API from OpenWeatherMap
get_weather_data <- function() {
  api_key <- "92f8cf043d0e8dbfd39d566f05d560f9"
  lat <- -37.8136 
  lon <- 144.9631  
  url <- paste0("https://api.openweathermap.org/data/3.0/onecall?lat=", lat, "&lon=", lon, "&appid=", api_key)
  response <- GET(url)
  weather_data <- content(response, "parsed")
  return(weather_data)
}

displayTableau <- reactive({
  input$place_of_interest == "memorials-and-sculptures"
})

################
#User Interface#
################
ui <- dashboardPage(
  dashboardHeader(title = "City of Melbourne"
                  ),
  skin = "blue",
  dashboardSidebar(
    header=setUpTableauInShiny(),
    # Selection bar of places of interest
    radioButtons(
      "place_of_interest",
      label = "Place of Interest",
      choices = c(
        "Museums and Art Galleries" = "art-galleries-and-museums",
        "Churches" = "churches",
        "Clubs" = "clubs",
        "Entertainments and Theaters" = "entertainment-and-theatre",
        "Memorials and Sculptures" = "memorials-and-sculptures",
        "Parks and Outdoor Facilities" = "parks-and-sports-facilities",
        "Food" = "cafes-and-restaurants",
        "Toilets" = "public-toilets"
      ),
      inline = FALSE,
      selected = NULL
    ),
    # Selection bar of public transport
    radioButtons(
      "public_transport",
      label = "Public Transport",
      choices = c("Bus", "Tram", "Taxi"),
      inline = FALSE,
      selected = NULL
    ),
    # real-time weather box
    infoBox(
      "Melbourne Weather",
      width = 12,
      icon = icon("cloud"),
      verbatimTextOutput("weather_info")
    )
  ),
  
  # main map
  dashboardBody(
    fluidRow(
      box(
        title = "Melbourne Tourist Guide",
        width = 12,
        leafletOutput("map", height = "400px")
        
      ),
      box(
        width = 12,
        title = "Information of Point of Interest",
        verbatimTextOutput("info")
      ),
      box(
        title = "Memorials and Sculptures",
        height = 575,
        id='memorialsInfo',
        tableauPublicViz(
          id='memorialsInfo',
          url='https://public.tableau.com/views/memorials_dashboard/memorialsandsculpturesdashboard?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
        )
      ),
      box (
        title = "Food Services",
        height = 575,
        id='restaurantsInfo',
        tableauPublicViz(
          id='restaurantsInfo',
          url = 'https://public.tableau.com/views/restaurants_dashboard/CafeandRestaurantsdashboard?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
        )
      )
    )
  )
)


################
# SHINY SERVER #
################
server <- function(input, output, session) {
  observeEvent(input$tableauAnalysis, {
    runjs('dispatchEvent(new Event("resize"))')
  })

  # identify the selection of public transport from users 
  filtered_data <- reactive({
    transport_type <- input$public_transport
    data <- data.frame(Latitude = numeric(0), Longitude = numeric(0))
    if ("Tram" %in% transport_type) {
      data <- rbind(data, tram_data)
    }
    if ("Taxi" %in% transport_type) {
      data <- rbind(data, taxi_data)
    }
    if ("Bus" %in% transport_type) {
      data <- rbind(data, bus_data)
    }
    return(data)
  })
  
  # identify the selection of POI from users 
  getType <- reactive({
    Poi <- c(paste("cleaned_data/", input$place_of_interest, ".csv", sep=""))
    Poi_icon <- c(paste("icons/", input$place_of_interest, ".png", sep=""))
    selected_data <- data.frame(Poi, Poi_icon)
    return(selected_data)
  })
  
  # identify the selection of POI from users agian for the POI description
  selected_data <- reactive({
    file_path <- paste0("cleaned_data/", input$place_of_interest, ".csv")
    data <- read.csv(file_path)
    data$source <- paste0(input$place_of_interest, ".csv")
    return(data)
  })
  
  # display user selected data
  display_column <- reactive({
    switch(input$place_of_interest,
           "memorials-and-sculptures" = "title",
           "name")  # default display "name"
  })
  
  getInfoText <- function(data) {
    
    if(nrow(data) == 0 || is.null(data$source) || length(data$source) != 1) {
      return("No data available or data source incorrect.")
    }
    data <- data[1,]
    switch(data$source,
           "cafes-and-restaurants.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type, "\n",
             "Address: ", data$address, "\n",
             "Seating Type: ", data$seating_type, "\n",
             "Number of Seats: ", data$number_of_seats,
             sep = ""
           ),
           "churches.csv" = paste(
             "Name: ", data$name,
             sep = ""
           ),
           "art-galleries-and-museums.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type,
             sep = ""
           ),
           "visitors-centres.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type,
             sep = ""
           ),
           "clubs.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type, "\n",
             "Address: ", data$address,
             sep = ""
           ),
           "parks-and-sports-facilities.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type,
             sep = ""
           ),
           "memorials-and-sculptures.csv" = paste(
             "Title: ", data$title, "\n",
             "Type: ", data$type, "\n",
             "Makers: ", data$makers, "\n",
             "Art Date: ", data$art_date, "\n",
             "Description: ", data$description, "\n",
             "Location: ", data$location, "\n",
             sep = ""  # Added sep parameter to remove spaces between text and values
           ),
           "public-toilets.csv" = paste(
             "Type: ", data$type, "\n",
             "Address: ", data$address, "\n",
             "Male: ", data$male, "\n",
             "Female: ", data$female, "\n",
             "Wheelchair Accessible: ", data$wheelchair, "\n",
             "Baby Facilities: ", data$baby_facil, "\n",
             sep = ""
           ),
           "entertainment-and-theatre.csv" = paste(
             "Name: ", data$name, "\n",
             "Type: ", data$type, "\n",
             "Address: ", data$address,
             if (!is.na(data$number_of_seats) && data$number_of_seats != "") paste("\nNumber of Seats: ", data$number_of_seats),
             sep = ""
           ),
           paste("Name: ", data$name, "\n",  # default case
                 "Type: ", data$type),
           sep = ""
    )
  }
  
  # Create a responsive object for creating maps and adding markers for the public transport
  create_map <- reactive({
    data <- filtered_data()
    
    # obtain the map of Melbourne using API from MapBox
    m <- leaflet(data = data) %>%
      addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}",
               options = leafletOptions(
                 id = "mapbox/streets-v11",
                 accessToken = "sk.eyJ1Ijoieml4dWFuMTFpbiIsImEiOiJjbG5rZWFiaGoxeWhlMmptbmdvcDk2cDFrIn0.07KXMUaR9auk4FkDhIW5NA"
               )) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 12) %>%
      setMaxBounds(lng1 = 144.85, lat1 = -37.89, lng2 = 145.08, lat2 = -37.73)
    
    if ("Tram" %in% input$public_transport) {
      m <- m %>% addMarkers(data = tram_stop_data, lng = ~Longitude, lat = ~Latitude, 
                            icon = tram_stop_icon, popup = ~name)
      for (i in 1:length(tram_track_data)) {
        m <- m %>% addGeoJSON(geojson = tram_track_data[i], color = "grey", weight = 2)
      }
    }
    if ("Taxi" %in% input$public_transport) {
      m <- m %>% addMarkers(data = taxi_data, lng = ~Longitude, lat = ~Latitude, 
                            icon = taxi_icon, popup = ~loc_desc)
    }
    if ("Bus" %in% input$public_transport) {
      m <- m %>% addMarkers(data = bus_data, lng = ~Longitude, lat = ~Latitude, icon = bus_icon)
    }
    
    # Create a new map layer for the visitor center
    m <- m %>% addMarkers(data = visitorCentres_data, lng = ~longitude, lat = ~latitude,
                          popup = ~name,
                          icon = visitorCentres_icon)
    
    if (!is.null(input$place_of_interest)) {
      # Check if any Place of Interest (POI) is selected
      if (length(input$place_of_interest) > 0) {
        poi_icon <- makeIcon(
          as.character(getType()$Poi_icon),
          iconWidth = 12, iconHeight = 12
        )
        
        # Create a new map layer for the selected type of place of interest
        m <- m %>% addMarkers(data = selected_data(), lng = ~longitude, lat = ~latitude,
                              popup = selected_data()[, display_column()],
                              icon = poi_icon)
      }
    }
    
    m
  })
  
  # main map output
  output$map <- renderLeaflet({
    create_map()
  })
  
  observeEvent(input$place_of_interest, {
    output$info <- renderText({ "" })  # clear previous selected data
  })
  
  observeEvent(input$map_marker_click, {
    clicked_marker <- input$map_marker_click
    matching_rows <- which(selected_data()$latitude == clicked_marker$lat & selected_data()$longitude == clicked_marker$lng)
    if(length(matching_rows) > 0) {
      data <- selected_data()[matching_rows[1],]
      info_text <- getInfoText(data)  
      output$info <- renderText({ info_text })
    } else {
      output$info <- renderText({ "Select a point of interest to display more information here." })
    }
  })
  
  # real-time weather output
  weather_data <- get_weather_data()
  output$weather_info <- renderPrint({
    # extract temperature and weather conditions from weather_data
    current_temp <- weather_data$current$temp
    current_conditions <- weather_data$current$weather[[1]]$main
    
    # create the weather text
    weather_info_text <- paste0(round((current_temp - 273.15)), "°C,", 
                                current_conditions)
    
    # delete "NULL"
    weather_info_text <- unlist(strsplit(weather_info_text, "NULL"))[1]
    
    cat(weather_info_text)
  })
}

#############
# RUN SHINY #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))

