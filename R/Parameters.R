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
    #' @field removal the rate of transfer from I to R
    removal = NULL,
    #' @field susceptibility A vector of size *nage* for susceptibility adjustment
    susceptibility = NULL,
    #' @field beta the transmission probability upon contact with an infected (retro computed from R0)
    beta = NULL,
    #' @field R0 the basic reproduction number
    R0 = NULL,
    #' @field contact the contact matrix
    contact = NULL,
    #' @description
    #' Create a new `Parameters` object.
    #' @param R0 R0 (=3)
    #' @return A new `Parameters` object.
    initialize = function(R0 = 3){
      self$preImmune = 0
      self$preExposed = 0
      #if preInfected >1 then it's the number of infected individuals (seeds)
      #if it's <1 then it's the proportion of infected
      self$preInfected = 10
      self$symptomatic = 1
      #number of age-groups
      self$nage = 18
      
      #model parameters
      #from E to I -> 6 days
      self$progression = 1/6
      #from I to R -> 6 days
      self$removal = 1/6
      
      self$R0 = R0
      
      #to be updated with true values
      self$contact = contact_matrix
      
      eig = eigen(self$contact)
      
      #retro compute the beta
      self$beta = R0 * self$removal/max(Re(eig$values))
      
      self$susceptibility = rep(1, self$nage)
      
    }
  )
)

