
library(devtools)
library(httr)
library(dygraphs)
library(tidyr)
library(threadr)
library(readr)

# Install package
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
library(sp)
library(rgdal)
library(raster)

# original DEVELOPEMENT verison of Leaflet on GitHub
# devtools::install_github('rstudio/leaflet')


setwd("C:/worldmet")
# setwd("C:/RICARDO-AEA/worldmet")
Sys.time()
source("get_temp_DB.R")

# dir <- "C:/worldmet/county_regions"
### shapefile for counties in England
# shp <- readOGR(dsn = dir, layer = "county_region")
# read GeoJSON-------------------------------------------------------------
counties <- readOGR("C:/worldmet/county_regions/county_region.geojson", "OGRGeoJSON")  # counties
plot(counties)
districts <- readOGR("C:/worldmet/district_borough_unitary_region/district_borough_unitary_region.geojson", "OGRGeoJSON")  # districts
plot(districts)

# ----- Transform to EPSG 4326 - WGS84 (required)
counties <- spTransform(counties, CRS("+init=epsg:4326"))
districts <- spTransform(districts, CRS("+init=epsg:4326"))
# names(shp)



############################################################################################################
####### Temperatrure data from RICARDO - AEA (AURN)#########################################################

# info_sites_temp_ARCHIVE <- search_database("archive", "AT10|AT25", extra = TRUE)    
  
info_sites_temp_ARCHIVE <- search_database("archive", "V10|V25", extra = TRUE) ### sites where external temperature is measured (TEOM-FDMS)
info_sites_ARCHIVE_temp <- subset(info_sites_temp_ARCHIVE, variable_friendly %in% c("v10", "v25")) 

info_sites_temp_ARCHIVE <- search_database("archive", "at10|at25", extra = TRUE) ### Atmospherich temperature sensors
info_sites_ARCHIVE_temp <- subset(info_sites_temp_ARCHIVE, variable_friendly %in% c("at10", "at25")) 

Archive_temp_sites <- cbind(info_sites_ARCHIVE_temp$site_name, info_sites_ARCHIVE_temp$longitude, info_sites_ARCHIVE_temp$latitude)
Archive_temp_sites <- as.data.frame(Archive_temp_sites)
# remove duplicates
Archive_temp_sites <- unique(Archive_temp_sites)
colnames(Archive_temp_sites) <- c("site_name", "lon", "lat")
write.csv(as.data.frame(Archive_temp_sites), "Archive_temp_sites.csv")

site_vector_temp <- unique(info_sites_temp_ARCHIVE$site)


# import all temperatrua data from TEOM PM2.5 and TEOM PM10
temperature_data_ARCHIVE <- import_measures("archive",site = site_vector_temp, 
                                    variable = c("at10", "at25"),
                                    start = "2016-06-30", 
                                   # end = "2016-06-29",
                                    extra = TRUE)
# remove NA values
temperature_data_ARCHIVE <- temperature_data_ARCHIVE[!(is.na(temperature_data_ARCHIVE$value)), ]

write.csv(temperature_data_ARCHIVE, "temperature_data_ARCHIVE.csv")

# select only the latest temperature for each variable (at25 and at10)
temperature_data_ARCHIVE_hour <- temperature_data_ARCHIVE %>%
   mutate(date = ymd_hms(date_end, tz = "UTC"),
          hour = hour(date)) %>%
   arrange(site_name, -hour, variable) %>%
# filter(variable == "at25") %>%
  group_by(site_name,
           site,
           variable,
           day = str_sub(date,start = 1, end = -10)) %>%
  summarize(max_hour = max(hour), temperature = head(value,1))

# make average of latest temperature between site at25 and site at10
temperature_data_ARCHIVE_hour <- temperature_data_ARCHIVE_hour %>%
  group_by(site_name,
           site,
           day) %>%
  summarize(latest_temperature = mean(temperature)) %>%
  ungroup()


# join data for lon and lat
temperature_data_ARCHIVE_hour <- temperature_data_ARCHIVE_hour %>%
  left_join(Archive_temp_sites, "site_name")


write.csv(temperature_data_ARCHIVE_hour, "temperature_data_ARCHIVE_hour.csv")
temperature_data_ARCHIVE_hour <- read.csv("temperature_data_ARCHIVE_hour.csv")


# make a leaflet map with site loaction only
popup_temp <- paste("<p><strong>", temperature_data_ARCHIVE_hour$site_name,":", "</strong>", sep = " ",
                     round(temperature_data_ARCHIVE_hour$latest_temperature), " &#8451")

map <- leaflet(data = Archive_temp_sites[,]) %>% 
  setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~popup_temp)
map


saveWidget(map,
           file="UK_Temp_sites_Ricardo.html",
           selfcontained = FALSE)


#### interroggate one specific site to make a dygraph plot ------------------------------------
# Site string
site <- "abd" # oxford St. Ebbes


# # Get data
time_series <- get_measurement_time_series(site)
# ts <- time_series$at10
# plot <- dygraph(ts)

interactive_plot(time_series$at10, site) #time series of air temperature where we have TEOM PM10
interactive_plot(time_series$at25, site) #time series of air temperature where we have TEOM PM2.5







############################################################################################################
########## WORLDMET DATA ###################################################################################
############################################################################################################
############################################################################################################

Archive_temp_sites <- read.csv("Archive_temp_sites.csv")

# extract all codes of meteorological monitoring sites close to UK air monitoring sites (AURN)
# bind all code together

code_meta_UK <- data.frame()

# get a list of all active meteorogical monitoring stations
# for (i in 1:32)  {
for (i in 1:nrow(Archive_temp_sites))  {
  AAA <- getMeta(lat = Archive_temp_sites$lat[i], lon =Archive_temp_sites$lon[i])
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

for (i in 1:nrow(code_meta_UK))  {
  #  for (i in 1:1)  {
  temp <- importNOAA(code = as.character(code_meta_UK[i,1]), year = 2016)
  temp <- temp %>%
    dplyr:: select(date,
                   code,
                   station,
                   lat,
                   lon,
                   air_temp,
                   RH)
  temp <- na.omit(temp)   # remove data with lat, lon == NA
  temp <- temp %>%
    mutate(date = ymd_hms(date, tz = "UTC"),
           day = date(date),
           hour = hour(date)) %>%
    arrange(-hour) 
  temp <- filter(temp, day == "2016-06-28") 
  
  if  (nrow(temp) == 0) {next} # it jumps to next step
  
  temp <- temp %>%
    group_by(day,
             lat,
             lon) %>%  # filter data for a selected day
    summarise(max_hour = max(hour), latest_temperature = head(air_temp,1))
  
  temp <- as.data.frame(temp)
  
  temp <- cbind(temp, code_meta_UK$station[i])
  #  temp <- cbind(temp, as.data.frame(code_meta_UK$station[i]))
  temp_UK <- rbind(temp_UK, data.frame(temp))
  i
}

# temp_new <- cbind(temp_uk, code_meta_UK$station)
colnames(temp_UK) <- c("day","lat", "lon", "hour", "latest_temperature", "station")

temp_UK <- unique(temp_UK)
write.csv(temp_UK, "temp_UK_2016_06_28.csv")



#### make a map with leaflet-------------------------------------------------
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





###########################################################################################################
###########################################################################################################
# counting points inside counties and disctricts (councils)

crs <- projection(counties) ### get projections from the county Geojson file

# make a spatial points dataframe with the points temperature data

temperature_data_ARCHIVE_hour <- temperature_data_ARCHIVE_hour %>%
  dplyr::select(latest_temperature,
         lon,
         lat)


temp_UK <- temp_UK %>%
  dplyr::select(latest_temperature,
                lon,
                lat)


temperature_TOT <- rbind(temperature_data_ARCHIVE_hour,
                         temp_UK)


temperature_TOT <- SpatialPointsDataFrame(temperature_TOT[,2:3], temperature_TOT, 
                                                        proj4string=CRS(crs)) 
plot(counties)
plot(districts)
# plot(districts, add = TRUE, lwd=2, col="red")
plot(temperature_TOT, add=TRUE, lwd=2, col="red")

plot(temperature_TOT)




# find points into boundaies of counties-------------------------------------
pts.counties <- over(temperature_TOT, counties)
# add a count to each row
pts.counties$n <- 1

pts.counties_sum <- pts.counties %>%
  group_by(FILE_NAME) %>%
  summarize(UK_AIR_stations = sum(n))

pts.counties_sum <- pts.counties_sum %>%
  left_join(pts.counties, "FILE_NAME")
 
pts.counties_sum <- pts.counties_sum %>%
dplyr:: select(FILE_NAME,
         UK_AIR_stations,
         AREA_CODE,
         DESCRIPTIO)
pts.counties_sum <- unique(pts.counties_sum)
write.csv(pts.counties_sum, "UK_AIR_NOAA_Temp_counties.csv")



# find points into boundairies of districts--------------------------------
pts.districts <- over(temperature_TOT, districts)
# add a count to each row
pts.districts$n <- 1

pts.districts_sum <- pts.districts %>%
  group_by(FILE_NAME) %>%
  summarize(UK_AIR_stations = sum(n))

pts.districts_sum <- pts.districts_sum %>%
  left_join(pts.districts, "FILE_NAME")
 

pts.districts_sum <- pts.districts_sum %>%
  dplyr:: select(FILE_NAME,
                 UK_AIR_stations,
                 AREA_CODE,
                 DESCRIPTIO)
pts.districts_sum <- unique(pts.districts_sum)
write.csv(pts.districts_sum, "UK_AIR_NOAA_Temp_districts.csv")






