## define a PolyHosp object

#' R6 Class representing a set of Parameters
#' 
#' Parameters contains all the parameters related to the population and the epidemic
#' @importFrom R6 R6Class
#' @importFrom ggvoronoi voronoi_polygon
#' @import leaflet
#' @importFrom sp SpatialPointsDataFrame over
#' @docType class
#' @export
#' @keywords population
#' @return Object of \code{\link{R6Class}} with all the population data.
#' @format \code{\link{R6Class}} object.
#' @examples
#' pop = PolyHosp$new()
PolyHosp <- R6::R6Class("PolyHosp",
  public = list(
    #' @field nage number of age groups
    nage = 0,
    #' @field vorHBase hospital database containing all hospital information
    vorHBase = NULL,
    #' @field vorPolyHosp the voronoi polygons
    vorPolyHosp = NULL,
    #' @field vorPopFiness the population inside each polygons
    vorPopFiness = NULL,
    #' @field vorPopRegion the population inside each Region
    vorPopRegion = NULL,
    #' @description
    #' Create a new `PolyHosp` object.
    #' @param hospitals list of hospitals on which we build the voronoi diagram
    #' @return A new `PolyHosp` object.
    initialize = function(hospitals = NULL){
      if (is.null(hospitals)) hospitals = hospCovid
      #merge the hospital list with the data
      self$vorHBase = merge(hospitals, info_etab, 
                     by.x = "FINESS_GEO", 
                     by.y = "finess_ET", 
                     all.x = TRUE, 
                     all.y = FALSE)
      
      self$vorHBase = self$vorHBase[!(is.na(FINESS_GEO) | is.na(finess_EJ) | is.na(lat))]
      
      self$vorPolyHosp = ggvoronoi::voronoi_polygon(self$vorHBase, x = "lng", y = "lat", outline = outlineFr)
      
      #now we build the population inside each polygons
      #and within the regions
      private$build_voronoi_pop(self$vorPolyHosp)
    }, 
    
    #' @description 
    #' get the leaflet map
    #' @return a leaflet map with the polygons
    getMap = function(){
      map = self$vorPolyHosp %>%
        leaflet() %>%
        addTiles() %>%
        addPolylines() %>%
        addCircleMarkers(lng = ~lng, 
                         lat = ~lat, 
                         opacity = 0.8, 
                         radius = 2, 
                         color = "red",
                         popup = ~ paste(
                           RS,
                           "<br>",
                           FINESS_GEO,
                           "<br>",
                           "Region:",
                           Region)
                         )
      return(map)
    }, 
    
    #' @description 
    #' get the population from a specific polygon by Finess
    #' @param finess the finess of the specific polygon
    getPopFiness = function(finess){
      if (!any(self$vorPopFiness[,FINESS_Voronoi == finess]))
        stop("finess not found !")
      return(self$vorPopFiness[FINESS_Voronoi == finess])
    },
    #' @description 
    #' get the population from a specific Region by name
    #' @param region the finess of the specific polygon
    getPopRegion = function(region){
      if (!any(self$vorPopRegion[,Region == region]))
        stop("Region not found !")
      return(self$vorPopRegion[Region == region])
    }
  ),
  private = list(
    #build the population for each polygon (FINESS) and Region
    build_voronoi_pop = function(vorPolyHosp) {
      # Identification of the towns in each Voronoi polygon #
      data_towns <- copy(population_contact)
      towns <- SpatialPointsDataFrame(data_towns[,.(lng,lat)], 
                                      data = data_towns)
      data_towns[, c("FINESS_Voronoi","Region") := over(towns, vorPolyHosp)[, c("FINESS_GEO","Region")]]
      
      # Compute the population per age per Voronoi polygon #
      self$vorPopFiness <- data_towns[, lapply(.SD, sum),
                                .SDcols = grep("^AGE", names(data_towns)),
                                by = c("FINESS_Voronoi")]
      # Compute the population per age per Region #
      self$vorPopRegion <- data_towns[, lapply(.SD, sum),
                                .SDcols = grep("^AGE", names(data_towns)),
                                by = c("Region")]
    },
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
  )
)
    
  
