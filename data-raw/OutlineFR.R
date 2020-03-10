library(maptools)
library(maps)
library(sp)
library(rgeos)

map=map('france', plot=FALSE)
map_polygons<-map2SpatialPolygons(map, IDs = map$names)
map_polygons <- gBuffer(map_polygons, byid = TRUE, width = 0)
outlineFr <- unionSpatialPolygons(map_polygons, rep(1,
                                                    max(map_polygons@plotOrder)))

usethis::use_data(outlineFr, overwrite = TRUE)
