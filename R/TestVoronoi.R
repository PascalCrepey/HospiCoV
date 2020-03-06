library(ggplot2)
library(ggvoronoi)
library(ggmap)
library(openxlsx)
library(data.table)
library(stringr)
library(raster)
library(maps)
library(leaflet)
library(maptools)

# functions computing the voronoi tesselisations

voronoi_tessel = function() {
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
    
    return(vorPolyHosp)
}

voronoi_map = function(vorPolyHosp) {
    map = vorPolyHosp %>%
        leaflet() %>%
        addTiles() %>%
        addPolylines() %>%
        addCircleMarkers(lng = mHBase$lng, 
                         lat = mHBase$lat, 
                         opacity = 0.8, 
                         radius = 2, 
                         color = "red")
    return(map)
}

voronoi_pop = function(vorPolyHosp) {
# Identification of the towns in each Voronoi polygon #
    data_towns <- readRDS("./Data/Population.rds")
    towns <- SpatialPointsDataFrame(data_towns[,.(lng,lat)], 
                                    data = data_towns)
    data_towns[, FINESS_Voronoi := over(towns, vorPolyHosp)[, "FINESS_GEO"]]
    
                                        # Compute the population per age per sex per Voronoi polygon #
    pop_Voronoi <- data_towns[, lapply(.SD, sum),
                              .SDcols = grep("SEXE", names(data_towns)),
                              by = "FINESS_Voronoi"]
    return(pop_Voronoi)
}

# finess_to_merge : vector of tessels' FINESS to be merged
voronoi_tessel_merge = function(vorPolyHosp, finess_to_merge, name_merge) {
    # Creation of a vector of IDs #
    original_data <- as.data.table(vorPolyHosp@data)
    original_data[, ID := .I]
    vec_ids <- original_data[, ID]
    # Identification of the tessels to unite in the SpatialPolygonDataframe #
    index <- which(original_data[,FINESS_GEO] %in% finess_to_merge)
    new_index <- max(vec_ids) + 1
    vec_ids[index] <- new_index
    # Update of the data frame #
    new_data <- original_data[-index,]
    modified_data <- original_data[index,
                                   lapply(.SD, sum),
                                   .SDcols = c(grep("LIT", names(original_data)),
                                               grep("SEJHC", names(original_data)),
                                               grep("JOU", names(original_data)),
                                               grep("PLA", names(original_data)),
                                               grep("SEJHP", names(original_data)))]
    modified_data <- cbind(modified_data, 
                           t(spatial.median(original_data[index,c("lat","lng")])))
    modified_data[, ":=" ("Libelle" = name_merge,
                          "ID" = new_index,
                          "FINESS_GEO" = new_index)]
    
    # Merge the tessels #
    new_vorPolyHosp <- SpatialPolygonsDataFrame(unionSpatialPolygons(vorPolyHosp, vec_ids),
                                                data = rbind(new_data, 
                                                             modified_data,
                                                             fill = TRUE),
                                                match.ID = "ID")
    
    return(new_vorPolyHosp)
}

