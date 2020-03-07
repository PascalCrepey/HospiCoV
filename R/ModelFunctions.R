## Wrapper function running the C++ model

#' @import data.table
#' @export
runMod <- function(params, startyear, sname, 
                   population) {

  month = 1
  interval = 1
  start.date <- as.Date(paste0(startyear, "-", month, "-01"))
  dates <- c(start.date)
  latest.date <- start.date + interval
  while (!(data.table::year(latest.date) > data.table::year(start.date) &
           data.table::yday(latest.date) >= data.table::yday(start.date))) {
    dates <- c(dates, latest.date)
    latest.date <- latest.date + interval
  }

  ## set the initial state
  #set the vaccination coverage into the initial state
  initS = population * (1 - params$preImmune - params$preExposed - params$preInfected)
  initE = population * params$preExposed
  initI = population * params$preInfected
  initR = population * params$preImmune
  init = matrix(c(S = initS, 
                  E = initE, 
                  I = initI, 
                  R = initR))
  
  out = engine_run(params, init)
  
  res = data.table(t(out[c(1,(4 * params$nage + 2):(6 * params$nage + 1)),]))
  setnames(res,c("Time",paste0("newC",1:params$nage),paste0("N",1:params$nage)))

  #apply asymptomatic
  for (col in paste0("newC",1:params$nage))
    res[,(col) := res[[col]]*(params$symptomatic)]
  
  res[, Time := dates[1:.N]]
  res[, Scenario := sname]
  
  return(res)
}


