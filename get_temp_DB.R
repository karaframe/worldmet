
# # library(importr)
# library(dplyr)
# library(threadr)
# library(dygraphs)
# library(tidyr)
# library(devtools)
# # devtools::install_github("skgrange/threadr")


get_measurement_time_series <- function(site_code, month_past = 4, quarter = NA) {

info_sites_ARCHIVE_temp <- search_database("archive", "at10|at25", extra = TRUE) %>% ### sites where external temperature is measured (TEOM-FDMS) 
    filter(site_friendly == site_code,
           variable_friendly %in% c("at10", "at25"))

# Get start and end dates
if (!is.na(quarter)) {
  
  # Parse
  date_quarter <- parse_date_time(quarter, c("ymd", "dmy"))
  
  # Start date
  date_start <- floor_date(date_quarter, "quarter")
  # Preceding 2 weeks
  date_start <- date_start - weeks(2)
  
  # End date, addition is to ensure the quarter has been entered
  date_end <- ceiling_date(date_quarter + 1, "quarter")
  
  # Following two weeks too
  date_end <- date_end + weeks(2)
  
} else {
  
  # Get date range for query
  date_end <- Sys.Date()
  # n months in the past
  date_start <- date_end - months(month_past)
  
}

# To character for function
date_end <- as.character(date_end)
date_start <- as.character(date_start)


# Import hourly data
temperature_data_ARCHIVE <- import_measures("archive", site = site_code, 
                                            variable = c("at10", "at25"),
                                            start = date_start, 
                                         #   end = date_end,
                                            extra = TRUE) %>% 
  dplyr:: select(date,
                 site,
                 site_name,
                 variable,
                 value)

temperature_data_ARCHIVE <- temperature_data_ARCHIVE %>%
  mutate(date = ymd_hms(date, tz = "UTC"))
                                

temperature_data_ARCHIVE <- na.omit(temperature_data_ARCHIVE)  # remove rowa with NA values

temperature_data_ARCHIVE <- temperature_data_ARCHIVE %>%
  spread(variable, value)

# Build timeseries for plots
time_series <- data_frame_to_timeseries(temperature_data_ARCHIVE)

# Return
time_series

}



############################################################################
############################################################################


# to create grouped interactive dygraphs
interactive_plot <- function(ts, site) {
  
  if (!is.null(ts)) {
    
    # Get colour vector
    colour_vector <- threadr::ggplot2_colours(45)
    
  #  if (site == "chbo") {
      
      plot <- dygraph(ts) %>% 
        dyOptions(colors = colour_vector[1]) %>% 
        dySeries(label =  "Temperature") %>% 
        dyAxis("y", label = paste("<p><strong>Temperature", site,  " &#8451")) %>%
        dyRangeSelector()
      
   # }
    

    # Return
    plot
    
  }
  
}


interactive_map <- function(df) {
  
  # Map
  map <- leaflet() %>% 
    addTiles(group = "OpenStreetMap") %>% 
    addProviderTiles("Stamen.Toner", group = "Toner") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Images") %>% 
    addCircleMarkers(data = df, lng = ~ longitude, lat = ~ latitude, 
                     color = "tomato", fillOpacity = 0.8, 
                     popup = ~ site_name, group = "Sites") %>% 
    addLayersControl(baseGroups = c( "OpenStreetMap", "Toner", "Images"),
                     overlayGroups = "Sites")
  
  # Return
  map
  
}

