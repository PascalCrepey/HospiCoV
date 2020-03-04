library(ggplot2)
library(ggvoronoi)
library(ggmap)
library(openxlsx)
library(data.table)
library(stringr)
library(raster)
library(maps)
library(leaflet)

# functions computing the voronoi tesselisations
 
outlineFr = map_data(map = "world", region = "france")

#get the data
hospCovid = data.table(read.xlsx("Data/Hopitaux_covid.xlsx"))
hospCovid[, FINESS_GEO := as.character(FINESS_GEO)]
hospCovid[, FINESS_GEO := str_pad(FINESS_GEO, 9 , pad = "0")]
info_etab = fread("Data/info_etab.csv")

mHBase = merge(hospCovid, info_etab, 
               by.x = "FINESS_GEO", 
               by.y = "finess_ET", 
               all.x = TRUE, 
               all.y = FALSE)

mHBase = mHBase[!(is.na(FINESS_GEO) | is.na(finess_EJ) | is.na(lat))]

vorPolyHosp = voronoi_polygon(mHBase, x = "lng", y = "lat", outline = outlineFr)

vorPolyHosp %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(lng = mHBase$lng, 
                   lat = mHBase$lat, 
                   opacity = 0.8, 
                   radius = 2, 
                   color = "red")
  

