
library(rgdal)
library(dplyr)
library(leaflet)
library(sp)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(tidyr)
library(webshot)

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

# import dataframe with FLAG data from REGIME Assessment (John Stedmand)
REGIME_ASSM <- read.csv("Flags_Bulgaria.csv") 
AAA <- as.data.frame(shp_BG@data)

# join data
shp_BG@data <- shp_BG@data %>% 
  left_join(REGIME_ASSM, "LOCALID")

# Make up color factor for vBulgaria for each pollutant

require('RColorBrewer')
display.brewer.pal(11,'RdYlBu')
# previewColors(colorFactor("RdYlBu", domain = NULL), LETTERS[1:2])
# zones
factpal_zones <- colorFactor("RdYlBu", shp_BG$ZONECODE) 
factpal_zones <- colorFactor(c("#0000ff", "#ff0000", "#99cc99", "#ffa500", "#ffff00", "#ccccff"), shp_BG$ZONECODE)
# blue, red, green, "orange, yellow, azure

# RED is no, BLUES is yes
factpal_NOx_NO2 <- colorFactor(c("#ff0000", "#0000ff"), shp_BG$NOx_NO2) # it will be a factor with two color because I have only "yes" or "no"
factpal_PM10_PM25 <- colorFactor(c("#ff0000", "#0000ff"), shp_BG$PM10_PM25) 
factpal_CO <- colorFactor(c("#ff0000", "#0000ff"), shp_BG$CO)

factpal_CO <- colorFactor(c("#ff0000", "#0000ff"), shp_BG$CO)
factpal_SO2 <- colorFactor(c("#0000ff"), shp_BG$SO2) # all YES

factpal_Benzene <- colorFactor(c("#0000ff"), shp_BG$Benzene) # all YES
factpal_Ozone <- colorFactor(c("#ff0000"), shp_BG$Ozone) # all NO
factpal_Pb <- colorFactor(c("#0000ff"), shp_BG$Pb) # all YES

# Build leafleft maps for Bulgaria ##################################


# ZONECODE

BG_map_ZONES <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%

  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_zones(shp_BG$LOCALID),
              popup = ~shp_BG$LOCALID,
              group = "code") %>%
  addLegend("bottomright", pal = factpal_zones, values = ~ LOCALID,
            title = "ZONE CODE",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_ZONES

## This is the png creation part
saveWidget(BG_map_ZONES, 'ZONES_Bulgaria.html', selfcontained = FALSE)
webshot('ZONES_Bulgaria.html', file='ZONES_Bulgaria.png', vwidth = 1300, vheight = 900, 
        cliprect = 'viewport')

##########################################################################################################

# NOx & NO2

# add name of pollutant on the map
"h1 { font-size: 1px;}"

BG_map_NOx_NO2 <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><strong>NOx</sub> & NO<sub>2<sub></strong>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_NOx_NO2(shp_BG$NOx_NO2),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_NOx_NO2, values = ~NOx_NO2,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_NOx_NO2


## This is the png creation part
saveWidget(BG_map_NOx_NO2, 'Flags_Bulgaria_NOx_NO2.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_NOx_NO2.html', file='Flags_Bulgaria_NOx_NO2.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')


############################################################################################

# PM10 & PM2.5

BG_map_PM10_PM25 <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><strong>PM<sub>10</sub> & PM<sub>2.5<sub></strong>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_PM10_PM25(shp_BG$PM10_PM25),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_PM10_PM25, values = ~PM10_PM25,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_PM10_PM25


## This is the png creation part
saveWidget(BG_map_PM10_PM25, 'Flags_Bulgaria_PM10_PM25.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_PM10_PM25.html', file='Flags_Bulgaria_PM10_PM25.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')



############################################################################################

# CO

BG_map_CO <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><center><strong>CO</sub><center>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_CO(shp_BG$CO),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_CO, values = ~CO,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_CO


## This is the png creation part
saveWidget(BG_map_CO, 'Flags_Bulgaria_CO.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_CO.html', file='Flags_Bulgaria_CO.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')


############################################################################################

# SO2

BG_map_SO2 <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><center><strong>SO<sub>2</sub><center>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_SO2(shp_BG$SO2),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_SO2, values = ~SO2,
            title = "monitoring requirements met",
           labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_SO2


## This is the png creation part
saveWidget(BG_map_SO2, 'Flags_Bulgaria_SO2.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_SO2.html', file='Flags_Bulgaria_SO2.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')



############################################################################################

# Benzene

BG_map_Benzene <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><center><strong>Benzene</sub><center>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_Benzene(shp_BG$Benzene),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_Benzene, values = ~ Benzene,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_Benzene


## This is the png creation part
saveWidget(BG_map_Benzene, 'Flags_Bulgaria_Benzene.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_Benzene.html', file='Flags_Bulgaria_Benzene.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')


############################################################################################

# Ozone

BG_map_Ozone <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><center><strong>Ozone</sub><center>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_Ozone(shp_BG$Ozone),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_Ozone, values = ~ Ozone,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_Ozone


## This is the png creation part
saveWidget(BG_map_Ozone, 'Flags_Bulgaria_Ozone.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_Ozone.html', file='Flags_Bulgaria_Ozone.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')


############################################################################################

# Pb (Lead)

BG_map_Pb <- leaflet(shp_BG) %>%
  setView(25.31, 42.87, 8) %>%
  addPopups(25, 43.9, "<h1><center><strong>Pb</sub><center>",
            options = popupOptions(closeButton = FALSE)) %>%
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.5, weight = 1.5, color = "black",
              fillColor =  ~ factpal_Pb(shp_BG$Pb),
              popup = ~shp_BG$LOCALID,
              group = "random_code") %>%
  addLegend("bottomright", pal = factpal_Pb, values = ~ Pb,
            title = "monitoring requirements met",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("agglomerate"),
    options = layersControlOptions(collapsed = TRUE)) 

# Return
BG_map_Pb


## This is the png creation part
saveWidget(BG_map_Pb, 'Flags_Bulgaria_Pb.html', selfcontained = FALSE)
webshot('Flags_Bulgaria_Pb.html', file='Flags_Bulgaria_Pb.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')
