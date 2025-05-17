library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(lubridate)

# API key for Ambee
API_KEY <- "a5bb697d82877ebf34b43df5fe01b6b2ac743ff05833f77414b3020abd2147df"

# Function to get location data from place name with better error handling
get_location_data <- function(place_name) {
  tryCatch({
    if (missing(place_name) || nchar(trimws(place_name)) == 0) {
      stop("Place name cannot be empty")
    }
    
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", 
                  URLencode(place_name), 
                  "&format=json&limit=1&addressdetails=1")
    response <- GET(url, user_agent("GeoLookup/1.0"))
    
    if (http_status(response)$category != "Success") {
      stop("API request failed")
    }
    
    data <- fromJSON(content(response, "text"), simplifyVector = TRUE)
    
    if (length(data) == 0) {
      stop("No results found for this location")
    }
    
    result <- if (is.data.frame(data)) data[1, ] else data[[1]]
    
    # Extract state from address details if available
    state <- if (!is.null(result$address$state)) {
      result$address$state
    } else {
      display_parts <- strsplit(result$display_name, ",")[[1]]
      if (length(display_parts) >= 2) trimws(display_parts[2]) else "Unknown"
    }
    
    list(
      name = place_name,
      longitude = as.numeric(result$lon),
      latitude = as.numeric(result$lat),
      state = state,
      display_name = result$display_name,
      bounding_box = result$boundingbox
    )
  }, error = function(e) {
    message("Error in get_location_data: ", e$message)
    return(NULL)
  })
}

# Helper function for NULL coalescing - uncommented and implemented correctly
`%||%` <- function(x, y) if (is.null(x)) y else x

# Function to get air quality data with better error handling
get_air_quality <- function(place) {
  tryCatch({
    if (missing(place) || nchar(trimws(place)) == 0) {
      stop("Place name cannot be empty")
    }
    
    url <- paste0("https://api.ambeedata.com/latest/by-city?city=", URLencode(place))
    resp <- GET(url, add_headers("x-api-key" = API_KEY), timeout(10))
    
    if (status_code(resp) != 200) {
      stop(paste("API request failed with status:", status_code(resp)))
    }
    
    data <- content(resp, as = "parsed", type = "application/json")
    
    if (is.null(data$stations) || length(data$stations) == 0) {
      stop("No station data available")
    }
    
    s <- data$stations[[1]]
    
    data.frame(
      city = s$city,
      state = s$state,
      country = s$countryCode %||% NA,
      place = s$placeName,
      postalCode = s$postalCode %||% NA,
      lat = s$lat,
      lon = s$lng,
      updatedAt = as.POSIXct(s$updatedAt, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      AQI = s$AQI,
      pollutant = s$aqiInfo$pollutant %||% "Unknown",
      concentration = s$aqiInfo$concentration %||% NA,
      category = s$aqiInfo$category %||% "Unknown",
      PM25 = s$PM25,
      PM10 = s$PM10,
      CO = s$CO,
      NO2 = s$NO2,
      OZONE = s$OZONE,
      SO2 = s$SO2,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Error in get_air_quality: ", e$message)
    return(NULL)
  })
}

# Weather details API
get_weather_quality <- function(latitude, longitude, place = "Unknown") {
  library(jsonlite)
  library(httr)
  
  # Base URL and parameters
  base_url <- "https://api.open-meteo.com/v1/forecast"
  full_url <- paste0(
    base_url,
    "?latitude=", URLencode(as.character(latitude)),
    "&longitude=", URLencode(as.character(longitude)),
    "&current=temperature_2m,wind_speed_10m",
    "&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m"
  )
  
  # Try to make API call
  tryCatch({
    response <- fromJSON(full_url)
    
    data <- list(
      place = place,
      latitude = response$latitude,
      longitude = response$longitude,
      generationtime_ms = response$generationtime_ms,
      timezone = response$timezone,
      timezone_abbreviation = response$timezone_abbreviation,
      elevation = response$elevation,
      current_units = response$current_units,
      current = response$current,
      hourly_units = response$hourly_units,
      hourly = list(
        time = response$hourly$time,
        temperature_2m = response$hourly$temperature_2m,
        relative_humidity_2m = response$hourly$relative_humidity_2m,
        wind_speed_10m = response$hourly$wind_speed_10m
      ),
      fetched_at = Sys.time()
    )
    
    return(data)
    
  }, error = function(e) {
    warning(paste("API call failed for", place, ":", e$message))
    return(NULL)
  })
}

# Weather simple details with better error handling
get_small_detail <- function(latitude, longitude) {
  tryCatch({
    url <- paste0("https://api.airvisual.com/v2/nearest_city?lat=", 
                  latitude, "&lon=", longitude, 
                  "&key=98fedb84-d203-4234-916d-60cd9b825bae")
    
    response <- GET(url, timeout(10))
    
    if (status_code(response) != 200) {
      stop(paste("API request failed with status:", status_code(response)))
    }
    
    data <- content(response, as = "parsed")
    
    if (is.null(data$data)) {
      stop("No weather data available")
    }
    
    list(
      city = data$data$city,
      state = data$data$state,
      country = data$data$country,
      temperature = data$data$current$weather$tp,
      humidity = data$data$current$weather$hu,
      wind_speed = data$data$current$weather$ws,
      wind_direction = data$data$current$weather$wd,
      pressure = data$data$current$weather$pr,
      icon = data$data$current$weather$ic,
      aqi = data$data$current$pollution$aqius,
      aqi_pollutant = data$data$current$pollution$mainus
    )
  }, error = function(e) {
    message("Error in get_small_detail: ", e$message)
    return(NULL)
  })
}

# news api - updated with the key you provided
get_news_by_place <- function(place) {
  api_key <- "9ba6d716d60a7c4f92140b7c4a3ff2e8"  # Updated API key
  url <- paste0(
    "https://gnews.io/api/v4/search?",
    "q=", URLencode(place), "&",
    "lang=en&",
    "country=in&",
    "max=10&",
    "apikey=", api_key
  )
  
  tryCatch({
    response <- httr::GET(url, timeout(10))
    
    if (httr::status_code(response) != 200) {
      stop(paste("News API request failed with status:", httr::status_code(response)))
    }
    
    content_data <- httr::content(response, as = "text", encoding = "UTF-8")
    news_data <- jsonlite::fromJSON(content_data)
    
    if (is.null(news_data$articles) || length(news_data$articles) == 0) {
      return(data.frame(
        title = "No news found for this location",
        description = "Try another location or check back later",
        url = "",
        publishedAt = Sys.time(),
        source = "",
        image = "",
        stringsAsFactors = FALSE
      ))
    }
    
    articles <- news_data$articles
    
    # Return as a clean data frame
    df <- data.frame(
      title = articles$title,
      description = articles$description,
      url = articles$url,
      publishedAt = articles$publishedAt,
      source = articles$source$name,
      image = articles$image,
      stringsAsFactors = FALSE
    )
    
    return(df)
  }, error = function(e) {
    message("Error in get_news_by_place: ", e$message)
    return(data.frame(
      title = paste("Error fetching data:", e$message),
      description = "",
      url = "",
      publishedAt = Sys.time(),
      source = "",
      image = "",
      stringsAsFactors = FALSE
    ))
  })
}

# Weather forecast function
fetch_weather_data <- function(latitude, longitude) {
  api_key <- "792fb6cfbb64528615bf3fc92e4692c5"
  api_url <- paste0("https://api.openweathermap.org/data/2.5/forecast?lat=", latitude, "&lon=", longitude, "&appid=", api_key)
  response <- GET(api_url)
  content <- content(response, "text")
  weather_data <- fromJSON(content)
  
  forecasts <- weather_data$list
  forecasts$dt_txt <- ymd_hms(forecasts$dt_txt)
  forecasts$date <- as.Date(forecasts$dt_txt)
  forecasts$hour <- hour(forecasts$dt_txt)
  forecasts$day <- wday(forecasts$dt_txt, label = TRUE, abbr = FALSE)
  
  # Convert all temperature values from Kelvin to Celsius
  forecasts$temp <- forecasts$main$temp - 273.15
  forecasts$feels_like <- forecasts$main$feels_like - 273.15
  forecasts$temp_min <- forecasts$main$temp_min - 273.15  # Convert min temp
  forecasts$temp_max <- forecasts$main$temp_max - 273.15  # Convert max temp
  
  forecasts$humidity <- forecasts$main$humidity
  forecasts$pressure <- forecasts$main$pressure
  forecasts$wind_speed <- forecasts$wind$speed
  forecasts$wind_deg <- forecasts$wind$deg
  forecasts$clouds <- forecasts$clouds$all
  forecasts$weather_main <- sapply(forecasts$weather, function(x) x$main[1])
  forecasts$weather_desc <- sapply(forecasts$weather, function(x) x$description[1])
  forecasts$weather_icon <- sapply(forecasts$weather, function(x) x$icon[1])
  
  sunrise <- hour(as.POSIXct(weather_data$city$sunrise, origin = "1970-01-01"))
  sunset <- hour(as.POSIXct(weather_data$city$sunset, origin = "1970-01-01"))
  forecasts$day_night <- ifelse(forecasts$hour >= sunrise & forecasts$hour < sunset, "Day", "Night")
  
  return(list(forecasts = forecasts, city = weather_data$city))
}

# UI layout
ui <- dashboardPage(
  dashboardHeader(title = "GeoLookup"),
  dashboardSidebar(
    textInput("place_input", "Enter Place Name:", placeholder = "e.g. Delhi"),
    actionButton("search_btn", "Search", icon = icon("search")),
    sidebarMenu(
      menuItem("Location Info", tabName = "locationinfo", icon = icon("map-marker-alt")),
      menuItem("Current AQI", tabName = "currentaq", icon = icon("smog")),
      menuItem("Weather Info", tabName = "weatherinfo", icon = icon("cloud")),
      menuItem("Advance Info", tabName = "advanceinfo", icon = icon("cloud-rain")),
      menuItem("Latest News", tabName = "latestnews", icon = icon("newspaper"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("locationinfo",
              fluidRow(
                box(title = "Location Details", width = 12, status = "primary",
                    textOutput("location_name"),
                    textOutput("location_latitude"),
                    textOutput("location_longitude"),
                    textOutput("location_state"),
                    textOutput("location_display_name")
                ),
                box(title = "Map", width = 12, status = "primary",
                    leafletOutput("map"))
              )
      ),
      tabItem("currentaq",
              fluidRow(
                box(plotlyOutput("pollutant_bar"), width = 6, title = "Pollutant Levels"),
                box(plotlyOutput("aqi_gauge"), width = 6, title = "AQI Index"),
                box(plotlyOutput("pm_line_chart"), width = 6, title = "PM2.5 and PM10"),
                box(plotlyOutput("pollutant_pie"), width = 6, title = "Pollutant Proportions"),
                box(
                  title = "Pollutant Descriptions", 
                  width = 12,
                  status = "info",
                  div(
                    h4("PM2.5 (Particulate Matter ≤ 2.5 micrometers)"),
                    p("Fine inhalable particles with diameters generally 2.5 micrometers and smaller. These particles can be inhaled deeply into the lungs and may cause respiratory problems."),
                    hr(),
                    h4("PM10 (Particulate Matter ≤ 10 micrometers)"),
                    p("Particulate matter with a diameter of 10 micrometers or smaller. These particles can enter the lungs and cause respiratory issues over time."),
                    hr(),
                    h4("CO (Carbon Monoxide)"),
                    p("A colorless, odorless gas produced by the incomplete combustion of fossil fuels. In high concentrations, it can be harmful to the body by reducing the blood's ability to carry oxygen."),
                    hr(),
                    h4("NO2 (Nitrogen Dioxide)"),
                    p("A reddish-brown gas produced by the burning of fossil fuels. It can irritate the respiratory system and contribute to the formation of acid rain and smog."),
                    hr(),
                    h4("OZONE (O₃)"),
                    p("A gas composed of three oxygen atoms. While ozone protects against UV rays in the upper atmosphere, ground-level ozone is harmful to the lungs and a major component of smog."),
                    hr(),
                    h4("SO2 (Sulfur Dioxide)"),
                    p("A colorless, pungent gas produced from burning fossil fuels that contain sulfur. It can cause respiratory issues and contributes to the formation of acid rain, affecting the environment.")
                  )
                )
              )
      ),
      # … inside dashboardBody( tabItems( … ) ) …
      
      # “Current Weather” tab
      tabItem(
        tabName = "weatherinfo",
        fluidRow(
          box(
            title = "Weather Details", 
            width = 12,
            textOutput("current_temp"),
            textOutput("current_humidity"),
            textOutput("current_wind"),
            textOutput("current_aqi")
          )
        ),
        fluidRow(
          box(plotlyOutput("temp_plot"),      width = 12, title = "Temperature Over Time"),
          box(plotlyOutput("humidity_plot"),  width = 12, title = "Humidity Over Time"),
          box(plotlyOutput("wind_plot"),      width = 12, title = "Wind Speed Over Time"),
          box(
            title = "Full Raw Data",
            width = 12,
            verbatimTextOutput("weather_data")
          )
        )
      ),
      
      # “Advance Info” 5‑day Forecast tab
      tabItem(
        tabName = "advanceinfo",
        fluidRow(
          box(
            width = 12,
            selectInput(
              "plot_type", 
              "Select Visualization:",
              choices = c(
                "Temperature Trend",
                "Temperature vs. Humidity",
                "Wind Speed and Direction",
                "Pressure Trend",
                "Cloud Cover",
                "Weather Conditions",
                "Daily Temperature Range"
              ),
              selected = "Temperature Trend"
            ),
            dateRangeInput(
              "date_range", 
              "Select Date Range:",
              start = Sys.Date(),
              end   = Sys.Date() + 4,
              min   = Sys.Date(),
              max   = Sys.Date() + 4
            ),
            checkboxInput("show_feels_like", "Show 'Feels Like' Temperature", value = TRUE),
            checkboxInput("show_points",     "Show Data Points",           value = FALSE),
            hr(),
            h4("City Information"),
            textOutput("city_info"),
            textOutput("sunrise_sunset")
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("weather_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            h4("Weather Summary"),
            tableOutput("weather_summary")
          )
        )
      )
      ,
      tabItem("latestnews",
              fluidRow(
                box(
                  title = "Latest News", 
                  width = 12, 
                  status = "primary",
                  uiOutput("news_list")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  rv <- reactiveValues(
    location_data = NULL,
    air_data = NULL,
    weather_data = NULL,
    weather_full = NULL,
    weather_forecast = NULL
  )
  
  observeEvent(input$search_btn, {
    place_name <- trimws(input$place_input)
    if (nchar(place_name) == 0) {
      showNotification("Please enter a place name", type = "error")
      return()
    }
    
    showModal(modalDialog("Fetching data...", footer = NULL))
    
    # Get location data
    loc_data <- get_location_data(place_name)
    if (is.null(loc_data)) {
      removeModal()
      showNotification("Failed to get location data", type = "error")
      return()
    }
    rv$location_data <- loc_data
    
    # Get air quality data
    aq_data <- get_air_quality(place_name)
    if (is.null(aq_data)) {
      showNotification("Failed to get air quality data", type = "warning")
    } else {
      rv$air_data <- aq_data
    }
    
    # Get weather data
    weather_data <- get_small_detail(loc_data$latitude, loc_data$longitude)
    if (is.null(weather_data)) {
      showNotification("Failed to get weather data", type = "warning")
    } else {
      rv$weather_data <- weather_data
    }
    
    # Get full weather data
    weather_full <- get_weather_quality(loc_data$latitude, loc_data$longitude, place_name)
    rv$weather_full <- weather_full
    
    # Get weather forecast data
    weather_forecast <- fetch_weather_data(loc_data$latitude, loc_data$longitude)
    rv$weather_forecast <- weather_forecast
    
    # Update date range inputs based on forecast data
    if (!is.null(weather_forecast)) {
      forecast_dates <- as.Date(weather_forecast$forecasts$dt_txt)
      min_date <- min(forecast_dates)
      max_date <- max(forecast_dates)
      
      updateDateRangeInput(session, "date_range",
                           start = min_date,
                           end = min(max_date, min_date + 4),
                           min = min_date,
                           max = max_date)
    }
    
    removeModal()
  })
  
  # Location outputs
  output$location_name <- renderText({ 
    req(rv$location_data)
    paste("Place Name:", rv$location_data$name) 
  })
  
  output$location_latitude <- renderText({ 
    req(rv$location_data)
    paste("Latitude:", rv$location_data$latitude) 
  })
  
  output$location_longitude <- renderText({ 
    req(rv$location_data)
    paste("Longitude:", rv$location_data$longitude) 
  })
  
  output$location_state <- renderText({ 
    req(rv$location_data)
    paste("Near Address:", rv$location_data$state) 
  })
  
  output$location_display_name <- renderText({ 
    req(rv$location_data)
    paste("Location :", rv$location_data$display_name) 
  })
  
  # Map output
  output$map <- renderLeaflet({
    req(rv$location_data)
    leaflet() %>%
      addTiles() %>%
      setView(lng = rv$location_data$longitude, 
              lat = rv$location_data$latitude, 
              zoom = 12) %>%
      addMarkers(lng = rv$location_data$longitude, 
                 lat = rv$location_data$latitude, 
                 popup = rv$location_data$display_name)
  })
  
  # AQI outputs
  output$pollutant_bar <- renderPlotly({
    req(rv$air_data)
    pollutants <- data.frame(
      Pollutant = c("PM2.5", "PM10", "NO2", "SO2", "OZONE", "CO"),
      Value = c(rv$air_data$PM25, rv$air_data$PM10, rv$air_data$NO2, 
                rv$air_data$SO2, rv$air_data$OZONE, rv$air_data$CO),
      Unit = c("µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "ppm")
    )
    
    plot_ly(pollutants, x = ~Pollutant, y = ~Value, type = "bar",
            text = ~paste(Value, Unit), hoverinfo = "text",
            marker = list(color = "rgba(55, 128, 191, 0.7)")) %>%
      layout(
        title = paste("Pollutant Levels in", rv$air_data$place),
        xaxis = list(title = ""),
        yaxis = list(title = "Concentration"),
        margin = list(b = 100)
      )
  })
  
  output$aqi_gauge <- renderPlotly({
    req(rv$air_data)
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = rv$air_data$AQI,
      title = list(text = paste("AQI -", rv$air_data$category), font = list(size = 18)),
      gauge = list(
        axis = list(range = list(0, 500), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, 50), color = "green"),
          list(range = c(50, 100), color = "yellow"),
          list(range = c(100, 150), color = "orange"),
          list(range = c(150, 200), color = "red"),
          list(range = c(200, 300), color = "purple"),
          list(range = c(300, 500), color = "maroon")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = rv$air_data$AQI
        )
      )
    ) %>% layout(margin = list(l = 20, r = 20))
  })
  
  output$pm_line_chart <- renderPlotly({
    req(rv$air_data)
    df <- data.frame(
      Time = "Current",
      PM25 = rv$air_data$PM25,
      PM10 = rv$air_data$PM10
    )
    
    plot_ly(df, x = ~Time, y = ~PM25, type = 'scatter', mode = 'markers', name = 'PM2.5') %>%
      add_trace(y = ~PM10, name = 'PM10') %>%
      layout(
        title = "PM2.5 and PM10 Levels",
        yaxis = list(title = "Concentration (µg/m³)"),
        showlegend = TRUE
      )
  })
  
  output$pollutant_pie <- renderPlotly({
    req(rv$air_data)
    pollutants <- data.frame(
      Pollutant = c("PM2.5", "PM10", "NO2", "SO2", "OZONE", "CO"),
      Value = c(rv$air_data$PM25, rv$air_data$PM10, rv$air_data$NO2, 
                rv$air_data$SO2, rv$air_data$OZONE, rv$air_data$CO)
    )
    
    plot_ly(pollutants, labels = ~Pollutant, values = ~Value, type = 'pie',
            textinfo = 'label+percent') %>%
      layout(title = "Pollutant Proportions")
  })
  
  # Weather outputs
  output$current_temp <- renderText({
    req(rv$weather_data)
    paste("Temperature:", rv$weather_data$temperature, "°C")
  })
  
  output$current_humidity <- renderText({
    req(rv$weather_data)
    paste("Humidity:", rv$weather_data$humidity, "%")
  })
  
  output$current_wind <- renderText({
    req(rv$weather_data)
    paste("Wind Speed:", rv$weather_data$wind_speed, "m/s | Direction:", rv$weather_data$wind_direction, "°")
  })
  
  output$current_aqi <- renderText({
    req(rv$weather_data)
    paste("Air Quality Index:", rv$weather_data$aqi, "(", rv$weather_data$aqi_pollutant, ")")
  })
  
  # Weather plots (Temperature, Humidity, Wind Speed)
  output$temp_plot <- renderPlotly({
    req(rv$weather_full)
    df <- data.frame(
      Time = as.POSIXct(rv$weather_full$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      Temperature = rv$weather_full$hourly$temperature_2m
    )
    
    plot_ly(df, x = ~Time, y = ~Temperature, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'red')) %>%
      layout(title = "Hourly Temperature", yaxis = list(title = "°C"))
  })
  
  output$humidity_plot <- renderPlotly({
    req(rv$weather_full)
    df <- data.frame(
      Time = as.POSIXct(rv$weather_full$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      Humidity = rv$weather_full$hourly$relative_humidity_2m
    )
    
    plot_ly(df, x = ~Time, y = ~Humidity, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue')) %>%
      layout(title = "Hourly Humidity", yaxis = list(title = "%"))
  })
  
  output$wind_plot <- renderPlotly({
    req(rv$weather_full)
    df <- data.frame(
      Time = as.POSIXct(rv$weather_full$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      Wind = rv$weather_full$hourly$wind_speed_10m
    )
    
    plot_ly(df, x = ~Time, y = ~Wind, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'green')) %>%
      layout(title = "Hourly Wind Speed", yaxis = list(title = "m/s"))
  })
  
  output$weather_data <- renderPrint({
    req(rv$weather_full)
    str(rv$weather_full)
  })
  
  # Weather forecast visualization
  # City Info
  output$city_info <- renderText({
    req(rv$weather_forecast)
    city <- rv$weather_forecast$city
    paste0(city$name, ", ", city$country, 
           " (Lat: ", round(city$coord$lat, 2), 
           ", Lon: ", round(city$coord$lon, 2), ")")
  })
  
  # Sunrise / Sunset
  output$sunrise_sunset <- renderText({
    req(rv$weather_forecast, rv$weather_forecast$city)
    
    # Get city data
    city <- rv$weather_forecast$city
    
    # Convert timestamps with error handling
    sunrise <- tryCatch({
      as.POSIXct(city$sunrise, origin = "1970-01-01", tz = "UTC")
    }, error = function(e) NA)
    
    sunset <- tryCatch({
      as.POSIXct(city$sunset, origin = "1970-01-01", tz = "UTC")
    }, error = function(e) NA)
    
    # Check if conversion succeeded
    if (any(is.na(sunrise), is.na(sunset))) {
      return("Sunrise/sunset data not available")
    }
    
    # Convert to local timezone (change to your preferred timezone)
    local_tz <- Sys.timezone() # or specify like "Asia/Kolkata"
    sunrise_local <- format(sunrise, "%H:%M %p", tz = local_tz)
    sunset_local <- format(sunset, "%H:%M %p", tz = local_tz)
    
    # Return formatted string
    paste0(
      "🌅 Sunrise: ", sunrise_local, 
      " | 🌇 Sunset: ", sunset_local,
      " (Local Time)"
    )
  })
  
  output$weather_plot <- renderPlotly({
    req(rv$weather_forecast, input$date_range)
    req(rv$weather_full, input$date_range)
    
    # Convert hourly data to data frame for plotting
    hourly_data <- data.frame(
      dt_txt = as.POSIXct(rv$weather_full$hourly$time, format = "%Y-%m-%dT%H:%M"),
      date = as.Date(rv$weather_full$hourly$time),
      hour = hour(as.POSIXct(rv$weather_full$hourly$time)),
      temp = rv$weather_full$hourly$temperature_2m,
      humidity = rv$weather_full$hourly$relative_humidity_2m,
      wind_speed = rv$weather_full$hourly$wind_speed_10m,
      # Add calculated fields
      day = weekdays(as.Date(rv$weather_full$hourly$time)),
      # Since Open-Meteo doesn't provide these, use reasonable defaults
      feels_like = rv$weather_full$hourly$temperature_2m # Simplified feels_like as same as temp
    )
    
    # Filter data for the selected date range
    filtered_data <- hourly_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    forecasts <- rv$weather_forecast$forecasts %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (nrow(forecasts) == 0) {
      return(ggplotly(ggplot() +
                        geom_text(aes(0.5, 0.5, label = "No data available for selected date range")) +
                        theme_void()))
    }
    
    if (nrow(filtered_data) == 0) {
      return(ggplotly(ggplot() +
                        geom_text(aes(0.5, 0.5, label = "No data available for selected date range")) +
                        theme_void()))
    }
    
    base_plot <- ggplot(filtered_data, aes(x = dt_txt)) +
      scale_x_datetime(labels = date_format("%a %H:%M")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    base_plot_forecasts <- ggplot(forecasts, aes(x = dt_txt)) +
      scale_x_datetime(labels = date_format("%a %H:%M")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    p <- NULL
    
    if (input$plot_type == "Temperature Trend") {
      p <- base_plot +
        geom_line(aes(y = temp, color = "Actual Temperature"), size = 1) +
        {if (input$show_feels_like) geom_line(aes(y = feels_like, color = "Feels Like"), linetype = "dashed")} +
        {if (input$show_points) geom_point(aes(y = temp, color = "Actual Temperature"))} +
        {if (input$show_points && input$show_feels_like) geom_point(aes(y = feels_like, color = "Feels Like"))} +
        scale_color_manual(values = c("Actual Temperature" = "red", "Feels Like" = "orange")) +
        labs(title = "Temperature Trend", y = "Temperature (°C)", x = "Date and Time", color = "Metric")
      
    } else if (input$plot_type == "Temperature vs. Humidity") {
      p <- ggplot(filtered_data, aes(x = temp, y = humidity, color = hour)) +
        geom_point(size = 3) +
        scale_color_gradient(low = "blue", high = "red") +
        labs(title = "Temperature vs. Humidity", x = "Temperature (°C)", y = "Humidity (%)", color = "Hour of Day") +
        theme_minimal()
      
    } else if (input$plot_type == "Wind Speed and Direction") {
      # Since Open-Meteo doesn't provide wind direction, modify this plot
      p <- ggplot(forecasts, aes(x = dt_txt, y = wind_speed, color = as.factor(wind_deg))) +
        geom_segment(aes(xend = dt_txt, yend = 0), size = 1) +
        geom_point(size = 2) +
        scale_color_discrete(name = "Wind Direction (degrees)") +
        labs(title = "Wind Speed and Direction", x = "Date and Time", y = "Wind Speed (m/s)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Pressure Trend") {
      p <- base_plot_forecasts +
        geom_line(aes(y = pressure, color = "Pressure"), size = 1) +
        {if (input$show_points) geom_point(aes(y = pressure, color = "Pressure"))} +
        scale_color_manual(values = c("Pressure" = "blue")) +
        labs(title = "Atmospheric Pressure Trend", y = "Pressure (hPa)", x = "Date and Time")
      
    } else if (input$plot_type == "Cloud Cover") {
      p <- base_plot_forecasts +
        geom_area(aes(y = clouds, fill = "Cloud Cover"), alpha = 0.5) +
        geom_line(aes(y = clouds, color = "Cloud Cover"), size = 1) +
        {if (input$show_points) geom_point(aes(y = clouds, color = "Cloud Cover"))} +
        scale_fill_manual(values = c("Cloud Cover" = "grey")) +
        scale_color_manual(values = c("Cloud Cover" = "black")) +
        labs(title = "Cloud Cover", y = "Cloud Coverage (%)", x = "Date and Time")
      
    } else if (input$plot_type == "Weather Conditions") {
      p <- ggplot(forecasts, aes(x = dt_txt, fill = weather_main)) +
        geom_bar(position = "stack") +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Weather Conditions Over Time", x = "Date and Time", y = "Count", fill = "Weather Condition") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Daily Temperature Range") {
      # Calculate daily temperature ranges from the hourly data
      daily_summary <- filtered_data %>%
        group_by(date) %>%
        summarize(
          min_temp = min(temp),
          max_temp = max(temp),
          avg_temp = mean(temp)
        )
      
      p <- ggplot(daily_summary, aes(x = date)) +
        geom_linerange(aes(ymin = min_temp, ymax = max_temp), size = 1, color = "blue") +
        geom_point(aes(y = avg_temp), size = 3, color = "red") +
        labs(title = "Daily Temperature Range", x = "Date", y = "Temperature (°C)") +
        theme_minimal()
    }
    
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  output$weather_summary <- renderTable({
    # Validate required inputs
    req(rv$weather_forecast, rv$weather_full, input$date_range)
    
    # Convert hourly data to data frame for precise measurements
    hourly_data <- data.frame(
      date = as.Date(rv$weather_full$hourly$time),
      temp = rv$weather_full$hourly$temperature_2m,
      humidity = rv$weather_full$hourly$relative_humidity_2m,
      wind_speed = rv$weather_full$hourly$wind_speed_10m
    )
    
    # Get forecast data for weather descriptions
    forecasts <- rv$weather_forecast$forecasts %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      mutate(date = as.Date(date))  # Ensure date is in Date format
    
    # Filter hourly data for the selected date range
    filtered_data <- hourly_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    # Return message if no data available
    if (nrow(forecasts) == 0 || nrow(filtered_data) == 0) {
      return(data.frame(Message = "No data available for selected date range"))
    }
    
    # Calculate numerical summaries from hourly data
    temp_summary <- filtered_data %>%
      group_by(date) %>%
      summarize(
        `Avg Temp (°C)` = round(mean(temp, na.rm = TRUE), 1),
        `Min Temp (°C)` = round(min(temp, na.rm = TRUE), 1),
        `Max Temp (°C)` = round(max(temp, na.rm = TRUE), 1),
        `Avg Humidity (%)` = round(mean(humidity, na.rm = TRUE), 1),
        `Avg Wind Speed (m/s)` = round(mean(wind_speed, na.rm = TRUE), 1)
      ) %>%
      mutate(date = format(as.Date(date), "%Y-%m-%d"))  # Format date properly
    
    # Get most common weather from forecasts
    weather_summary <- forecasts %>%
      group_by(date) %>%
      summarize(
        `Most Common Weather` = ifelse(
          all(is.na(weather_main)), 
          "No data", 
          names(which.max(table(weather_main)))
        )
      ) %>%
      mutate(date = format(as.Date(date), "%Y-%m-%d"))  # Format date properly
    
    # Combine both summaries and ensure proper date display
    left_join(temp_summary, weather_summary, by = "date") %>%
      rename(Date = date) %>%  # Rename for clearer column header
      arrange(Date)  # Sort by date
  })
  
  
  # news data
  news_df <- eventReactive(input$search_btn, {
    req(input$place_input)
    get_news_by_place(input$place_input)
  })
  
  output$news_list <- renderUI({
    req(news_df())
    articles <- news_df()
    
    if (nrow(articles) == 0) {
      return(tags$p("No news available for this location."))
    }
    
    card_list <- lapply(seq_len(nrow(articles)), function(i) {
      art <- articles[i, ]
      tags$div(
        class = "news-card", 
        style = "margin-bottom:20px; padding:15px; border:1px solid #ddd; border-radius:8px;",
        
        tags$h4(
          tags$a(href = art$url, art$title, target = "_blank")
        ),
        
        if (!is.na(art$image) && nzchar(art$image)) {
          tags$img(src = art$image,
                   style = "width:100%; max-height:200px; object-fit:cover; margin-bottom:10px;")
        },
        
        tags$p(art$description),
        
        tags$p(
          tags$small(
            paste0(
              "Source: ", art$source,
              " | Published: ",
              format(
                as.POSIXct(art$publishedAt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                "%b %d, %Y %H:%M"
              )
            )
          )
        )
      )
    })
    
    do.call(tagList, card_list)
  })
}

# Run the application
shinyApp(ui = ui, server = server)