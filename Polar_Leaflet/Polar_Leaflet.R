
library(openair)
library(importr)
library(leaflet)
library(worldmet)
library(dplyr) # at least version 0.5.0
library(plyr)
library(tidyr)

# options(viewer = NULL) 

# # Load package
# library(devtools)
# # Install package from webserver
# install_url("http://172.31.4.159/aqe-R-hub/packages/importr.zip")

# directory where polar plots will be written
# dir_polar <- "~/temp/polarplots/"
setwd("C:/Polar_Leaflet")


# site codes
codes <- c("kc1", "my1", "cll2", "lon6")
pollutant <- "no2"

# make sure they are in order
codes <- sort(codes)


data <- import_measures("archive", site = codes, 
                        variable = pollutant, 
                        start = 2015, end = 2015)

# # import data using importr
# data <- import_measures("archive", site = codes, 
#                         variable = pollutant, 
#                         start = 2015, end = 2015, extra = TRUE)


# # remove data without date
# data <- data[!is.na(data$date),]
# # data <- na.omit(data)  # remove row with NA values
# 
# data <- data %>%
#   select(date,
#          site,
#          value)
# colnames(data)[3] <- "no2"


# # Reshape to messy data
# data_db_wide <- data %>%
#   mutate(key = paste(site, variable, sep = "_"))%>%
#   select(date, key, value) %>%
#   spread(key, value)


# get lat/lon and site name
data_process <- search_database("archive")
data_process <- data_process %>%
  dplyr:: select(site, site_name, latitude, longitude)
data_process <- filter(data_process, site %in% codes)
data_process <- distinct(data_process, .keep_all = TRUE)
data_process <- arrange(data_process, site)

# import met data for London Heathrow
met <- importNOAA(year = 2015)

data <- inner_join(data, met, by = "date")
# data <- inner_join(data_db_wide, met, by = "date")



# check that directory is empty / exists
# if (dir.exists(dir_polar)) {
#   # remove existing files
#   files <- list.files(dir_polar, full.names = TRUE)
#   file.remove(files)
#   
# } else {
#   
#   dir.create(dir_polar)
#   
# }


# function to produce a polar plot, with transparent background
plot_polar <- function(data, pollutant, ...) {
  
  png(paste0("C:/Polar_Leaflet/", data$site[1], ".png"), width = 4 * 300, 
      height = 4 * 300, res = 300, bg = "transparent")
  
  polarPlot(data, pollutant = pollutant, key = FALSE, ...)
  
  dev.off()
  
}


# basic plot
# data(mydata)
# polarPlot(mydata, pollutant = "no2")

# go through all sites and make some plots
plyr::ddply(data, "site", plot_polar, pollutant, cols = "jet")



# definition of 'icons' aka the openair plots
leafIcons <- icons(
  iconUrl = list.files("C:/Polar_Leaflet/", full.names = TRUE),
  iconWidth = 150, iconHeight = 150
)

# Sys.setenv(https_proxy="https://harproxy02:3128")


# plot leaflet map

leaflet(data = data_process) %>% 
  addTiles() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  # addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addMarkers(~longitude, ~latitude, icon = leafIcons, popup = ~site_name) %>%
  addScaleBar(options = scaleBarOptions(imperial = TRUE))




