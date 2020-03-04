library(ggplot2)
library(ggvoronoi)
library(ggmap)
library(openxlsx)
library(data.table)
library(stringr)
library(raster)
library(maps)

# functions computing the voronoi tesselisations

France = get_map(c(left = -5, top = 51.3, right = 8.5, bottom = 41.7), 
                 source = "google")

 
outlineFr = map_data(map = "world", region = "france")


ggmap(France)

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

ggmap(France) + geom_path(data = fortify_voronoi(vorPolyHosp), 
                          )
  
ggmap(France, base_layer = ggplot(data = mHBase,aes(x = lng, y = lat))) +
  stat_voronoi( geom = "path" , outline = outlineFr, color = "blue") +
  geom_point(size = 1, color = "red") 
    
  
  geom_path(data = fortify_voronoi(vorPolyHosp), aes(x,y))
