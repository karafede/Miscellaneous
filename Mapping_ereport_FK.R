
library(rgdal)
library(dplyr)
library(leaflet)
library(sp)
library(raster)
library(maptools)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(devtoos)
library(httr)
library(RCurl)
library(threadr)
library(tidyr)
library(ereportr)
library(importr)
library(gissr)
library(webshot)

# Romania
file_zones <- "http://cdr.eionet.europa.eu/ro/eu/aqd/b/envvxdf8g/REP_D-RO_ANPM_20160414_B-003.xml"
file_regimes <- "http://cdr.eionet.europa.eu/ro/eu/aqd/c/envvnpkvq/REP_D-RO_ANPM_20151222_C-001.xml"
file_methods_measures <- "http://cdr.eionet.europa.eu/ro/eu/aqd/d/envvhoxdw/REP_D-RO_ANPM_20151006_D-004.xml" 

## Italy
# file_zones <- "http://cdr.eionet.europa.eu/it/eu/aqd/b_preliminary/envuvsew/B_IT_Zones_prelim.xml"
file_zones <- "http://cdr.eionet.europa.eu/it/eu/aqd/b/envvnq8kq/IT_B_2014_retro_20151223_110558.xml"
file_methods_measures <- "http://cdr.eionet.europa.eu/it/eu/aqd/d/envvnrduq/IT_D_2014_retro_20151223_110753.xml"

#file_regimes <- "U:/R-code/example/italy/IT_C_2014_retro_20151223_144549.xml"
#file_methods_measures <- "http://cdr.eionet.europa.eu/it/eu/aqd/d/envvnrduq/IT_D_2014_retro.xml"
#file_methods_models <- "xml_storage/GB_D_ModelledAssessmentMethods.json"
#file_e1a <- "xml_storage/E1a_GB_FixedObservations_11-02-2016.xml.bz2"
#file_attainment <- "http://172.31.4.157/archive/e-reporting/aq/G_GB_Attainment.xml"

## Bulagaria
file_zones <- "http://cdr.eionet.europa.eu/bg/eu/aqd/b/envvwzc2w/REP_D-BG_BG-ExEA_20160412_B1.xml"
file_regimes <- "http://cdr.eionet.europa.eu/bg/eu/aqd/c/envvgky9g/REP_D-BG_BG-ExEA_20150929_C-2909.xml"
file_methods_measures <- "http://cdr.eionet.europa.eu/bg/eu/aqd/d/envvufagg/REP_D-BG_BG-ExEA_20160315_D-001.xml"
#file_methods_models <- ""
file_e1a <- "http://cdr.eionet.europa.eu/bg/eu/aqd/e1a/envvubcsq/REP_D-BG_BG-ExEA_20160315_E-001.xml"
file_attainment <- "http://cdr.eionet.europa.eu/bg/eu/aqd/g/envvhdag/REP_D-BG_BG-ExEA_20151015_G_v2.xml"
# http://cdr.eionet.europa.eu/bg/eu/aqd/g/envvw12dg/REP_D-BG_BG-ExEA_20160412_G-001.xml


# Sweden
file_zones <- "http://cdr.eionet.europa.eu/se/eu/aqd/b/envvughwg/REP_SE_20160315_B.xml"
file_regimes <- "http://cdr.eionet.europa.eu/se/eu/aqd/c/envvfkhqq/REP_SE_20150914_C.xml"
file_methods_measures <- "http://cdr.eionet.europa.eu/se/eu/aqd/d/envvnj55w/REP_SE_20151222_D.xml"
#file_e1a
file_attainment <- "http://cdr.eionet.europa.eu/se/eu/aqd/g/envvfwitq/REP_SE_20150914_G.xml"


##########################################################################################################
########## MAPPING STUFF #################################################################################

# Load attainment polygons using ereport package
# Load zones using ereport package
# Load monitoring sites using ereport package

# UK
sp_attainment_UK <- read_xml_spatial(file = "C:/ereportr/UK_XML/G_GB_Attainment.xml", document = "attainment")  # from file_attainment
sp_zones_UK <- read_xml_spatial(file = "C:/ereportr/UK_XML/B_GB_Zones_retro.xml", document = "zones")   # from file_zones  
sp_stations_UK <- read_xml_spatial(file = "C:/ereportr/UK_XML/GB_D_FixedAssessmentMethods.xml", document = "stations") # file_methods_measures


# Sweden 
sp_stations_SE <- read_xml_spatial(file = "C:/ereportr/SE_XML/REP_SE_20151222_D.xml", document = "stations")  # file_methods_measures
# sp_zones_SE <- read_xml_spatial(file = "C:/ereportr/SE_XML/REP_SE_20160315_B.xml", document = "zones")   # from file_zones 

# Bulgaria
sp_stations_BG <- read_xml_spatial(file = "C:/ereportr/BG_XML/REP_D-BG_BG-ExEA_20160315_D-001.xml", document = "stations") # file_methods_measures


# Italy
sp_zones_IT <- read_xml_spatial(file = "C:/ereportr/IT_XML/IT_B_2014_retro_20151223_110558.xml", document = "zones")   # from file_zones  
sp_stations_IT <- read_xml_spatial(file = "C:/ereportr/IT_XML/IT_D_2014_retro_20151223_110753.xml", document = "stations") # file_methods_measures



# Plot with leaflet using ereport package

#UK
map_attainment_UK <- build_map(sp_attainment_UK, "attainment")
map_zones_UK <- build_map(sp_zones_UK, "zones")
map_stations_UK <- build_map(sp_stations_UK, "stations")

# Sweden
map_stations_SE <- build_map(sp_stations_SE, "stations")

# Bulgaria
map_stations_BG <- build_map(sp_stations_BG, "stations")

# Italy
map_stations_IT <- build_map(sp_stations_IT, "stations")

map_zones_IT <- build_map(sp_zones_IT, "zones")
map_zones_IT <- map_zones_IT %>%
  setView(12.49, 41.9, 5)
map_zones_IT



###### making a separate leaflet map for the zones of Sweden using shp files from EIONET #####

###### Sweden  ##################################################################### 
# Set working directory
setwd("C:/ereportr/SE_XML")
dir <- "C:/ereportr/SE_XML/shp_SE"

### shapefile for zones in Sweden
shp_SE <- readOGR(dsn = dir, layer = "SE_Zones_2014")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_SE <- spTransform(shp_SE, CRS("+init=epsg:4326"))

plot(shp_SE)

names(shp_SE)
head(shp_SE)
shp_SE@data
shp_SE@data$ag_nonag

# make a random dataframe
df_ID <- shp_SE$zonecode
df_data <- c("yes","yes","no","no","yes","yes")
df <- data.frame(df_ID, df_data) 
colnames(df) <- c("zonecode", "random")
# df$AAA <- df$random=="yes"

shp_SE@data <- shp_SE@data %>% 
  left_join(df, "zonecode")


# subset only agglomerated data
shp_SE_ag <- subset(shp_SE, ag_nonag =="ag")
shp_SE_ag@data

# subset only agglomerated data
shp_SE_nonag <- subset(shp_SE, ag_nonag =="nonag")
shp_SE_nonag@data

head(shp_SE)
# Make up some random levels for all Sweden

# previewColors(colorFactor("RdYlBu", domain = NULL), LETTERS[1:2])
factpal <- colorFactor("RdYlBu", shp_SE$random)

 map_SE_zones <- leaflet(shp_SE) %>%
   setView(13.15, 59.4, 5) %>%
   # Base maps
   addTiles(group = "OSM (default)") %>%
   addPolygons(data = shp_SE, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.7,
               color =  ~factpal(shp_SE$random),
               popup = ~shp_SE$random,
               group = "agglomerate") %>%
 addLegend("bottomright", pal = factpal, values = ~random,
           title = "Random",
           labFormat = labelFormat(prefix = ""),
           opacity = 1)
 map_SE_zones
 

# Make up some random levels for all "ag" polygons
factpal_ag <- colorFactor("RdYlBu", shp_SE_ag$zonecode)

# Make up some random levels for all "ag" polygons
factpal_nonag <- colorFactor("RdYlBu", shp_SE_ag$zonecode)

# Build leafleft map ##################################

map_SE_zones <- leaflet(shp_SE) %>%
  setView(13.15, 59.4, 5) %>%
   # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(data = shp_SE_ag, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color =  ~factpal_ag(shp_SE_ag$zonecode),
              popup = ~shp_SE_ag$zonecode,
              group = "agglomerate") %>%
  addPolygons(data = shp_SE_nonag, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.6,
              color =  ~factpal_nonag(shp_SE_nonag$zonecode),
              popup = ~shp_SE_nonag$zonecode,
              group = "non agglomerate") %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate", "non agglomerate"),
                      options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("non agglomerate")

# Return
map_SE_zones




###### Bulgaria  ##################################################################### 
# Set working directory
setwd("C:/ereportr/BG_XML")
dir <- "C:/ereportr/BG_XML/shp_BG"

### shapefile for zones in Sweden
shp_BG <- readOGR(dsn = dir, layer = "BG_aq_zone_2014")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_BG <- spTransform(shp_BG, CRS("+init=epsg:4326"))

plot(shp_BG)
names(shp_BG)
shp_BG@data

#  df_ID <- shp_BG$ZONECODE
#  df_data <- c("yes","no","yes","no","no","yes")
#  df <- data.frame(df_ID, df_data) 
#  colnames(df) <- c("ZONECODE", "random")
#  df$AAA <- df$random=="yes"

#  shp_BG@data <- shp_BG@data %>% 
# left_join(df, "ZONECODE")


# import dataframe with FLAG data from REGIME Assessment (John Stedmand)
REGIME_ASSM <- read.csv("Flags_Bulgaria.csv") 
AAA <- as.data.frame(shp_BG@data)

shp_BG@data <- shp_BG@data %>% 
  left_join(REGIME_ASSM, "LOCALID")

# Make up some color factor for vBulgaria

require('RColorBrewer')
display.brewer.pal(11,'RdYlBu')
# previewColors(colorFactor("RdYlBu", domain = NULL), LETTERS[1:2])
factpal_NOx <- colorFactor("RdYlBu", shp_BG$NOx_NO2) # it will be a factor with two color because I have only "yes" or "no"



# Build leafleft map for Bulgaria ##################################

# add name of pollutant on the map
"h1 { font-size: 40px;}"
# content <- '<h1>NO<sub>2</sub><h1>'
content <- '<h1>NOx_NO2</sub><h1>'

BG_map_NOx <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 7) %>%
  addPopups(22.7, 44.19, content,
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.6, weight = 1.5, color = "black",
              fillColor =  ~ factpal_NOx(shp_BG$NOx_NO2),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_NOx, values = ~NOx_NO2,
            title = "FLAGS",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 
# %>%
 # hideGroup("agglomerate")

# Return
BG_map_NOx


## This is the png creation part
saveWidget(BG_map_NOx, 'Flags_Bulgaria_NOx.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_NOx.html', file='Flags_Bulgaria_NOx.png',
        cliprect = 'viewport')



######################################################################
#### save into into html files

saveWidget(map_stations_SE,
           file="C:/ereportr/SE_XML/Stations_Sweden.html",
           selfcontained = FALSE)

saveWidget(map_SE_zones,
           file="C:/ereportr/SE_XML/Zones_Sweden.html",
           selfcontained = FALSE)

saveWidget(map_stations_BG,
           file="C:/ereportr/BG_XML/Stations_Bulgaria.html",
           selfcontained = FALSE)

saveWidget(map_BG_zones,
           file="C:/ereportr/BG_XML/Zones_Bulgaria.html",
           selfcontained = FALSE)

saveWidget(map_zones_IT,
           file="C:/ereportr/IT_XML/Zone_Italy.html",
           selfcontained = FALSE)

saveWidget(map_stations_IT,
           file="C:/ereportr/IT_XML/stations_Italy.html",
           selfcontained = FALSE)

##########################################################################################################
##########################################################################################################
##########################################################################################################

sp_zones_UK
plot(sp_zones_UK)

# ----- Transform to EPSG 4326 - WGS84 (required)
sp_zones_UK <- spTransform(sp_zones_UK, CRS("+init=epsg:4326"))

# make a random dataframe
df_ID <- sp_zones_UK$localid
df_data <- sp_zones_UK$zone_type
df <- data.frame(df_ID, df_data) 
colnames(df) <- c("localid", "random")
# df$AAA <- df$random=="yes"
# df$random <- ifelse(grepl("agg",ignore.case = FALSE, df$random, fixed = TRUE), 
#                                  "NO", df$random)
df$random <- gsub("agg", "YES", df$random)
df$random <- gsub("noYES", "NO", df$random)

sp_zones_UK@data <- sp_zones_UK@data %>% 
  left_join(df, "localid")

factpal <- colorFactor("RdYlBu", sp_zones_UK$random)

map_UK_zones <- leaflet(sp_zones_UK) %>%
  addTiles(group = "OSM (default)") %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color =  ~factpal(sp_zones_UK$random),
              popup = ~sp_zones_UK$random,
              group = "agglomerate") %>%
addLegend("bottomright", pal = factpal, values = ~random,
          title = "Random",
          opacity = 1)
map_UK_zones


