## define a parameter object

#' R6 Class representing a set of Parameters
#' 
#' Parameters contains all the parameters related to the epidemic
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import ggplot2
#' @export
#' @keywords parameter
#' @return Object of \code{\link{R6Class}} with all the parameters related to the population and the epidemic.
#' @format \code{\link{R6Class}} object.
#' @examples
#' params = Parameters$new()
#'   
Parameters <- R6::R6Class("Parameters",
  public = list(
    #' @field nage number of age groups
    nage = 0,
    #' @field preImmune the proportion of immune individuals at the beginning of the epidemic
    preImmune = NULL,
    #' @field preExposed the proportion of exposed individuals at the beginning of the epidemic
    preExposed = NULL,
    #' @field preInfected the proportion of exposed individuals at the beginning of the epidemic
    preInfected = NULL,
    #' @field symptomatic the proportion of symptomatic infection
    symptomatic = NULL,
    #' @field progression the rate of transfer from E to I
    progression = NULL,
    #' @field beta the transmission probability upon contact with an infected (retro computed from R0)
    beta = NULL,
    #' @field contact the contact matrix
    contact = NULL,
    #' @field agegroupnames the name of the agegroups
    agegroupnames = NULL,
    #' @description
    #' Create a new `Parameters` object.
    #' @param R0 R0 (=3)
    #' @return A new `Parameters` object.
    initialize = function(R0 = 2.2){
      self$preImmune = 0
      self$preExposed = 0
      #if preInfected >1 then it's the number of infected individuals (seeds)
      #if it's <1 then it's the proportion of infected
      self$preInfected = 10
      self$symptomatic = 1

      
      #model parameters
      #from E to I -> 5 days
      self$progression = 1/5.33
      #from I to R -> 11 days
      private$.removal = 1/10.9
      
      private$.R0 = R0
      
      #change calibrated susceptibility value
      if (R0 > 3) rsus = 3
      else if (R0 < 1.5 ) rsus = 1.5
      else rsus = round(R0, digits = 1)
      x = CalibSusceptibility[R0 == rsus, susceptibility]
      private$.susceptibility = c(x[1],x[1], 
                                  x[2],x[2],
                                  x[3],x[3],
                                  x[4],x[4],
                                  x[5],x[5],
                                  x[6],x[6],
                                  x[7],x[7],
                                  x[8],x[8],
                                  x[9])
      
      #to be updated with true values
      self$contact = contact_matrix
      
      #number of age-groups
      self$nage = nrow(contact_matrix)
      self$agegroupnames = colnames(contact_matrix)
      
      eig = eigen(private$.susceptibility * self$contact)
      
      #retro compute the beta
      self$beta = R0 * self$removal/max(Re(eig$values))
      
    },
    #' @description
    #' Create a new list containing the required parameters for the model
    #' 
    #' @return A new list of parameters.
    getList = function(){
      list(
        beta = self$beta,
        nage = self$nage,
        contact = self$contact,
        removal = private$.removal,
        progression = self$progression,
        preInfected = self$preInfected,
        symptomatic = self$symptomatic,
        preImmune = self$preImmune,
        preExposed = self$preExposed, 
        susceptibility = private$.susceptibility,
        agegroupnames = self$agegroupnames, 
        nbDays = private$.nbDays,
        R0 = private$.R0
      )
    }
  ), 
  active = list(
    #' @field susceptibility A vector of size *nage* for susceptibility adjustment
    susceptibility = function(x){
      if (missing(x)) {
        private$.susceptibility
      }else{
        stopifnot(is.vector(x), length(x) == self$nage || 
                    (length(x) == 9 && self$nage == 17))
        if (length(x) == 9) {
          #convenient since we calibrate on fewer age groups
          private$.susceptibility = c(x[1],x[1], 
                                      x[2],x[2],
                                      x[3],x[3],
                                      x[4],x[4],
                                      x[5],x[5],
                                      x[6],x[6],
                                      x[7],x[7],
                                      x[8],x[8],
                                      x[9])
        }else {
          private$.susceptibility = x
        }
        
        #update beta
        eig = eigen(private$.susceptibility * self$contact)
        self$beta = private$.R0 * private$.removal/max(Re(eig$values))
      }
    },
    #' @field R0 the basic reproduction number
    R0 = function(value) {
      if (missing(value)) {
        private$.R0
      }else{
        stopifnot(is.numeric(value))
        private$.R0 = value
        #change calibrated susceptibility value
        if (value > 3) rsus = 3
        else if (value < 1.5 ) rsus = 1.5
        else rsus = round(value, digits = 1)
        x = CalibSusceptibility[R0 == rsus, susceptibility]
        private$.susceptibility = c(x[1],x[1], 
                                    x[2],x[2],
                                    x[3],x[3],
                                    x[4],x[4],
                                    x[5],x[5],
                                    x[6],x[6],
                                    x[7],x[7],
                                    x[8],x[8],
                                    x[9])
        #retro compute the beta
        eig = eigen(private$.susceptibility * self$contact)
        self$beta = private$.R0 * private$.removal/max(Re(eig$values))
      }
    }, 
    #' @field removal the rate of transfer from I to R
    removal = function(x) {
      if (missing(x)) {
        private$.removal
      }else{
        stopifnot(x < 1)
        #update removal rate
        private$.removal = x
        
        #update beta
        eig = eigen(private$.susceptibility * self$contact)
        self$beta = private$.R0 * private$.removal/max(Re(eig$values))
      }
    },
    #' @field duration the duration fo the simulation i.e. c("Week", "Month", "Trimester", "Semester", "Year").
    duration = function(x){
      if (missing(x)) {
        private$.duration
      }else{
        if (!any(x %in% c("Week", "Month", "Trimester", "Semester", "Year")))
          stop("Allowed values are Week, Month, Trimester, Semester, and Year.")
        private$.nbDays = switch(x,
                                 "Week" = 7, 
                                 "Month" = 30, 
                                 "Trimester" = 90,
                                 "Semester" = 180, 
                                 "Year" = 364
        )
      }
    }, 
    #' @field nbDays the number of days of the simulation
    nbDays = function(x){
      if (missing(x)) {
        private$.nbDays
      }else{
        stopifnot(is.numeric(x))
        private$.nbDays = x
        private$.duration = switch(as.character(x),
                                 "7" = "Week", 
                                 "30" = "Month", 
                                 "90" = "Trimester",
                                 "180" = "Semester", 
                                 "364" = "Year",
                                 "Custom"
        )
      }
    }
  ), 
  private = list(
    .susceptibility = NULL,
    .R0 = NULL, 
    .duration = "Trimester",
    .nbDays = 90,
    .removal = NULL
  )
)

