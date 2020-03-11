# calibrate susceptibility per age
library(nloptr)
source("R/EpiData.R")

susceptibilityVector = rep(0.8,9)
lb = rep(0,9)
ub = rep(1,9)

ag10 = c(unlist(lapply(1:8, function(x) rep(x,2))),9)
#build parameter object
params = Parameters$new(R0 = 2.5)
vorhosp = PolyHosp$new()
pop = vorhosp$getPopRegion("Ile-de-France")

fitness = function(x, pop, params){
  params$susceptibility = x
  # c(x[1],x[1], 
  #                           x[2],x[2],
  #                           x[3],x[3],
  #                           x[4],x[4],
  #                           x[5],x[5],
  #                           x[6],x[6],
  #                           x[7],x[7],
  #                           x[8],x[8],
  #                           x[9])
  finalRes = runMod(params = params$getList(), sname = "calibration", population = pop)
  sumC = finalRes[, sum(Cases), by = "AgeGroup"]
  sumC[, AG10 := ag10]
  sumC10 = sumC[, sum(V1), by="AG10"]
  sumC10[, prop := V1/sum(V1)]
  sumC10[, data := CasesAgeDistribution]
  sumC10[, squerr := (prop - data)^2]
  return(sum(sumC10$squerr))
}

#mainCalib = lapply(2.8, function(r){
mainCalib = lapply(seq(1.5, 3, 0.1), function(r){
  params = Parameters$new(R0 = r)
  resCalib = nloptr(x0 = susceptibilityVector, 
                      lb = lb, 
                      ub = ub, 
                      params = params, 
                      pop = pop,
                      eval_f = fitness,
                      opts = list("algorithm" = "NLOPT_LN_SBPLX", 
                                  "print_level" = 0,
                                  "maxeval" = 5000,
                                  "xtol_rel" = 1e-5)
  )
  data.table(R0 = r, AG = 1:9, 
             susceptibility = resCalib$solution, 
             objective = resCalib$objective, 
             iter = resCalib$iterations)
})

CalibSusceptibility = rbindlist(mainCalib)

usethis::use_data(CalibSusceptibility, overwrite = TRUE)
# resCalib15$solution
# resCalib$solution
# params$susceptibility = resCalib$solution
# 
# finalRes = runMod(params = params$getList(), sname = "calibration", population = pop)
# sumC = finalRes[, sum(Cases), by = "AgeGroup"]
# sumC[, AG10 := ag10]
# sumC10 = sumC[, sum(V1), by="AG10"]
# sumC10[, prop := V1/sum(V1)]
# sumC10[, data := CasesAgeDistribution]
# sumC10[, squerr := (prop - data)^2]
# sumC10
