
library(devtools)
library(httr)
library(dygraphs)
library(tidyr)
library(threadr)
library(readr)

# Install packag
# inside Ricardo
# with_config(use_proxy("http://harproxy02:3128"), 
#             install_github("davidcarslaw/worldmet"))

# install_github("davidcarslaw/worldmet")
# Load packages
library(worldmet)
library(openair)
library(lubridate)
library(dplyr)
library(importr)
library(leaflet)
library(htmlwidgets)

# original DEVELOPEMENT verison of Leaflet on GitHub
# devtools::install_github('rstudio/leaflet')


setwd("C:/worldmet")
# setwd("C:/RICARDO-AEA/worldmet")
Sys.time()
source("get_temp_DB_old.R")

############################################################################################################
####### Temperatrure data from RICARDO - AEA (AURN)#########################################################

# info_sites_temp_ARCHIVE <- search_database("archive", "temp", extra = TRUE)    
  
info_sites_temp_ARCHIVE <- search_database("archive", "V10|V25", extra = TRUE) ### sites where external temperature is measured (TEOM-FDMS)
info_sites_ARCHIVE_temp <- subset(info_sites_temp_ARCHIVE, variable_friendly %in% c("v10", "v25")) 
# info_sites_ARCHIVE_temp <- subset(info_sites_temp_ARCHIVE, variable_friendly %in% c("temp")) 

Archive_temp_sites <- cbind(info_sites_ARCHIVE_temp$site_name, info_sites_ARCHIVE_temp$latitude, info_sites_ARCHIVE_temp$longitude)
Archive_temp_sites <- as.data.frame(Archive_temp_sites)
colnames(Archive_temp_sites) <- c("site_name", "Lat", "Lon")
write.csv(as.data.frame(Archive_temp_sites), "Archive_temp_sites.csv")

site_vector_temp <- unique(info_sites_temp_ARCHIVE$site)

temperature_data_ARCHIVE <- import_measures("archive",site = site_vector_temp, 
                                    variable = "temp",
                                    start = "2016-06-22", 
                                    end = "2016-06-23",
                                    extra = TRUE)

write.csv(temperature_data_ARCHIVE, "temperature_data_ARCHIVE.csv")

# find MAX temperature for each site
temperature_data_ARCHIVE <- temperature_data_ARCHIVE %>%
  group_by(site_name,
           site) %>%
  summarize (temp = max(value))

# join data for lat and long
temperature_data_ARCHIVE <- temperature_data_ARCHIVE %>%
  left_join(Archive_temp_sites, "site_name")


# make a leaflet map with site loaction only

popup_temp <- paste("<p><strong>", temperature_data_ARCHIVE$site_name,":", "</strong>", sep = " ",
                     round(temperature_data_ARCHIVE$temp), " &#8451")

map <- leaflet(data = Archive_temp_sites[,]) %>% 
  setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles() %>%
  addMarkers(~Lon, ~Lat, popup = ~popup_temp)
map


saveWidget(map,
           file="UK_Temp_sites_Ricardo.html",
           selfcontained = FALSE)


#### interroggate one specific site ------------------------------------
# Site string
site <- "abd" # oxford St. Ebbes


# # Get data
time_series <- get_measurement_time_series(site)
# ts <- time_series$temp
# plot <- dygraph(ts)

interactive_plot(time_series$temp, site)

############################################################################################################
############################################################################################################

Archive_temp_sites <- read.csv("Archive_temp_sites.csv")

# extract all codes of meteorological monitoring sites close to UK air monitoring sites (AURN)
# bind all code together

code_meta_UK <- data.frame()

# get a list of all active meteorogical monitoring stations
# for (i in 1:32)  {
for (i in 1:nrow(Archive_temp_sites))  {
   AAA <- getMeta(lat = Archive_temp_sites$Lat[i], lon =Archive_temp_sites$Lon[i])
   AAA <- AAA %>%
     mutate(date = ymd(END, tz = "UTC"),
            year = year(date),
            day = day(date)) %>%
     ungroup() %>%
     arrange(-year) 
   # code_uk <- AAA[1,]$code # get code from the first line for the importNOAA data
  #  date_uk <- as.character(AAA[1,]$date)
   code_uk <- cbind(AAA[1,]$code, as.character(AAA[1,]$date), as.character(AAA[1,]$STATION))
   code_meta_UK <- rbind(code_meta_UK, data.frame(code_uk))
}
   
colnames(code_meta_UK) <- c("code", "date", "station")


# filter some data base on  a given date date
# exclude variables v1, v2, v3
myvars <- code_meta_UK$date %in% c("2015-04-30", "2015-08-28", "2015-05-24", "2007-03-07")
code_meta_UK <- code_meta_UK[!myvars,]


# extract MAXIMUM temperature data for a chosen day, average them and bind them together
temp_UK <- data.frame()
# import data from a list of codes...by date (one day only)

# for (i in 1:nrow(code_meta_UK))  {
  for (i in 1:1)  {
  temp <- importNOAA(code = as.character(code_meta_UK[i,1]), year = 2016)
  temp <- temp %>%
    select(date,
           code,
           station,
           lat,
           lon,
           air_temp,
           RH)
  temp <- na.omit(temp)   # remove data with lat, lon == NA
  temp <- temp %>%
     mutate(date = ymd_hms(date, tz = "UTC"),
            day = date(date))
  temp <- filter(temp, day == "2016-06-22") 
 

  if  (nrow(temp) == 0) {next} # it jumps to next step

# temp$lat <- round(temp$lat, 2)
# temp$lon <- round(temp$lon, 2)

 temp <- temp %>%
    group_by(day,
             lat,
             lon) %>%  # filter data for a selected day
  summarise(MAX_air_temp = max(air_temp, na.rm = TRUE))
  
#  temp <- cbind(temp, as.data.frame(code_meta_UK$station[i]))
  temp_UK <- rbind(temp_UK, data.frame(temp))
 i
}
  
colnames(temp_UK) <- c("day","lat", "lon", "MAX_air_temp", "station")

temp_UK <- unique(temp_UK)
write.csv(temp_UK, "temp_UK_2016_06_22.csv")

###### load temperature data from worldmet #################################

temp_UK <- read.csv("temp_UK_2016_05_08.csv")
temp_UK$MAX_air_temp <-round(temp_UK$MAX_air_temp, 0) # round temperature data


popup_temp <- paste0("<p><strong> </strong>", 
                       temp_UK$MAX_air_temp)

# make a leaflet map with site loaction only

# map <- leaflet(data = temp_UK[,]) %>% 
#   setView(lng = -2, lat = 53.5, zoom = 6) %>%
#   addTiles() %>%
#   addMarkers(~lon, ~lat, popup = ~ station)
# map
# 
# 
# map <- leaflet(data = temp_UK[,]) %>% addTiles() %>%
#   addPopups(~lon, ~lat, ~as.character(temp_UK$AVG_air_temp), 
#             options = popupOptions(minWidth = 20, closeOnClick = FALSE, closeButton = FALSE))
# map


# Marker + Static Label using custom label options
map <- leaflet(data = temp_UK[,]) %>% addTiles() %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   weight = 3, radius=2, color = 'blue',
                   stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(temp_UK$MAX_air_temp),
                   labelOptions = labelOptions(noHide = T))
map



