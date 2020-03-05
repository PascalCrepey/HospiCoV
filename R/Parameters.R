## define a parameter object
library(R6)


Parameters <- R6Class("Parameters",
  public = list(
    nage = 0,
    preImmune = NULL,
    preExposed = NULL,
    preInfected = NULL,
    symptomatic = NULL,
    progression = NULL,
    susceptibility = NULL,
    beta = NULL,
    R0 = NULL,
    contact = NULL,
    initialize = function(R0 = 3){
      self$preImmune = 0
      self$preExposed = 0
      self$preInfected = 0
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
      self$contact = diag(1, nrow = self$nage, ncol = self$nage)
      
      eig = eigen(self$contact)
      
      #retro compute the beta
      self$beta = R0 * removal/max(Re(eig$values))
      
      self$susceptibility = rep(1, self$nage)
      
    }
  )
)

# gamma = 1/3        # recovery period of influenza in days^{-1}
# R0    = 1.5        # R0 of a hypothetical pandemic strain of influenza
# 
# C = matrix(0,nrow=nage,ncol=nage)
# C[1,1] = 18   # number contacts per day kids make with kids
# C[1,2] = 6    # number contacts per day kids make with adults (all kids have an adult in the home)
# C[2,1] = 3    # number contacts per day adults make with kids (not all adults have kids)
# C[2,2] = 12   # number contacts per day adults make with adults
# if (lcalculate_transmission_probability==1){
#   M = C
#   M[1,1] = C[1,1]*f[1]/f[1]
#   M[1,2] = C[1,2]*f[1]/f[2]
#   M[2,1] = C[2,1]*f[2]/f[1]
#   M[2,2] = C[2,2]*f[2]/f[2]
#   eig = eigen(M)
#   # reverse engineer beta from the R0 and gamma 
#   beta = R0*gamma/max(Re(eig$values))  
#   beta = beta
# }else{
#   beta = 0.05
# }