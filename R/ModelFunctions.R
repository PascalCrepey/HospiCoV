#' Wrapper function running the C++ model
#' @param params parameter object
#' @param sname name of the scenario
#' @param population population vector containing in the first cell the localization of the population
#' @param startDate the date of initial introduction of cases (Date object) (default: as.Date("2020-02-01"))
#' @import data.table
#' @export
runMod <- function(params, sname, 
                   population, startDate = as.Date("2020-02-01")) {
  
  #fix the starting date for cosmetic purposes
  dates = seq(startDate, startDate + params$nbDays, by = "day")
  
  #fix the scenario name
  loc = population[[1]]
  sname = paste0(sname," - ",loc)

  #get the population vector
  pop = unlist(population[,-1])
  ## set the initial state
  if (params$preInfected > 1) preInf = params$preInfected / sum(pop)
  else preInf = params$preInfected
  #set the initial seeds into the initial state
  initS = pop * (1 - params$preImmune - params$preExposed - preInf)
  initE = pop * params$preExposed
  initI = pop * preInf
  initR = pop * params$preImmune
  init = matrix(c(S = initS, 
                  E = initE, 
                  I = initI, 
                  R = initR))
  ### run the model 
  out = engine_run(params, init)
  
  res = as.data.table(out[,c(1,(4 * params$nage + 2):(6 * params$nage + 1))])
  setnames(res,c("Time",paste0("newI",1:params$nage),paste0("N",1:params$nage)))

  #apply asymptomatic
  # for (col in paste0("newC",1:params$nage))
  #   res[,(col) := res[[col]]*(params$symptomatic)]
  
  res[, Time := dates[1:.N]]
  res[, Scenario := sname]
  
  finalRes = melt(res, id.vars = "Time", 
                  measure.vars = patterns(c("^newI.*", "^N.*")),
                  variable.name = "AgeGroup", value.name = c("Infected", "N"))
  finalRes[, AgeGroup := factor(params$agegroupnames[AgeGroup], 
                                levels = params$agegroupnames)]
  
  return(finalRes)
}



#' Wrapper function running the C++ model multiple times
#' @param params parameter object
#' @param sname name of the scenario
#' @param population population vector containing in the first cell the localization of the population
#' @import data.table
#' @export
runMultiMod <- function(paramsObj, sname, 
                   population, nbIterations) {
  
  #fix the starting date for cosmetic purposes
  startyear = 2020
  month = 2
  start.date <- as.Date(paste0(startyear, "-", month, "-01"))
  dates = seq(start.date, start.date + params$nbDays, by = "day")
  
  #fix the scenario name
  loc = population[[1]]
  sname = paste0(sname," - ",loc)
  
  #get the population vector
  pop = unlist(population[,-1])
  ## set the initial state
  if (params$preInfected > 1) {
    preInf = params$preInfected / sum(pop)
  } else {
    preInf = params$preInfected
  }
  #set the vaccination coverage into the initial state
  initS = pop * (1 - params$preImmune - params$preExposed - preInf)
  initE = pop * params$preExposed
  initI = pop * preInf
  initR = pop * params$preImmune
  init = matrix(c(S = initS, 
                  E = initE, 
                  I = initI, 
                  R = initR))
  
  ### run the model 
  allRes = lapply(1:nbIterations, function(i){
    
    params = paramsObj$getList()
    out = engine_run(params, init)
    
    res = as.data.table(out[,c(1,(4 * params$nage + 2):(6 * params$nage + 1))])
    setnames(res,c("Time",paste0("newI",1:params$nage),paste0("N",1:params$nage)))
    
    res[, iter := i]
    
  })
  
  mainRes = rbindlist(allRes)
  
  #apply asymptomatic
  # for (col in paste0("newC",1:params$nage))
  #   res[,(col) := res[[col]]*(params$symptomatic)]
  
  mainRes[, Time := dates[Time]]
  mainRes[, Scenario := sname]
  
  finalRes = melt(mainRes, id.vars = c("Time","iter"), 
                  measure.vars = patterns(c("^newI.*", "^N.*")),
                  variable.name = "AgeGroup", value.name = c("Infected", "N"))
  finalRes[, AgeGroup := factor(params$agegroupnames[AgeGroup], 
                                levels = params$agegroupnames)]
  
  return(finalRes)
}

