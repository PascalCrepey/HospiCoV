## ------ RUN MODEL ----------------------------------------------------------
simulation <- function(locations, SimulationParameters, regions = TRUE) {

    all_res = lapply(locations, function(loc) {
        ## create Parameter
        params = Parameters$new(SimulationParameters$R0)
        
        if (regions) {
            params$preInfected = pre_infected[Region == loc, preInfected]
            pop = SimulationParameters$pHosp$getPopRegion(loc)
            startDate = pre_infected[Region == loc, Date]
        } else {
            params$preInfected = 10
            pop = SimulationParameters$pHosp$getPopFiness(loc)
            startDate = as.Date("2020-03-10")
        }
        
        #set duration
        params$duration = SimulationParameters$Duration

        #run the simulation
        finalRes = runMod(params = params$getList(), 
                          sname = SimulationParameters$sname, 
                          population = pop,
                          startDate = startDate)
        finalRes[, Region := loc]
        finalRes[, All := "All"]
    })
    out = rbindlist(all_res)
    return(out)
}

##------ Run model with specific R0 for all regions -----------------------------
run_model <- function(R0) {

    regions = pre_infected[!Region %in% c("France", "Metropole"), Region]

    SimulationParameters = list(R0 = R0,
                                Duration = "Trimester", 
                                Outcome = "Infected", 
                                sname = "test",
                                DaysHosp = 15,
                                DaysICU = 15,
                                DaysVentil = 15,
                                        #create Population
                                pHosp = PolyHosp$new(),
                                currDateHosp = as.Date("10/03/2020"),
                                ShowCaseTimeSeries = FALSE
                                )

    res = simulation(regions, SimulationParameters)

    outcomes = compute_outcomes(res,
                                severity_risk,
                                ICU_risk,
                                ventil_risks,
                                death_risk,
                                DaysHosp = SimulationParameters$DaysHosp,
                                DaysICU = SimulationParameters$DaysICU,
                                DaysVentil = SimulationParameters$DaysVentil)
    return(outcomes)
}

get_outcomes_byreg <- function(data) {
    ## Infected
    inf = data[, sum(Infected), by = Region]
    setnames(inf, old = "V1", new = "Infected")
    inf = rbind(inf, data.table(Region = "All", Infected = sum(inf$Infected)))
    
    ## Severe cases
    severe = data[, sum(severe), by = Region]
    setnames(severe, old = "V1", new = "Severe cases")
    severe = rbind(severe, data.table(Region = "All", "Severe cases" = sum(severe$`Severe cases`)))
    
    ## ICU hospit
    icu = data[, sum(ICU), by = Region]
    setnames(icu, old = "V1", new = "ICU")
    icu = rbind(icu, data.table(Region = "All", ICU = sum(icu$ICU)))
    
    ## Ventilation
    ventil = data[, sum(overall.ventil), by = Region]
    setnames(ventil, old = "V1", new = "Ventilation")
    ventil = rbind(ventil, data.table(Region = "All", Ventilation = sum(ventil$Ventilation)))
    
    ## Deaths
    deaths = data[, sum(deaths), by = Region]
    setnames(deaths, old = "V1", new = "Deaths")
    deaths = rbind(deaths, data.table(Region = "All", Deaths = sum(deaths$Deaths)))
    
    ## Combine tables
    outcomes_byreg = Reduce(merge, list(inf, severe, icu, ventil, deaths))

    return(outcomes_byreg)
}

get_bedreq_byreg <- function(data) {
    ## Hosp beds
    hosp = data[, sum(BedHosp), by = Region]
    setnames(hosp, old = "V1", new = "Hospital bed-days")
    hosp = rbind(hosp, data.table(Region = "All", "Hospital bed-days" = sum(hosp$`Hospital bed-days`)))
    ## ICU beds
    icu = data[, sum(BedICU), by = Region]
    setnames(icu, old = "V1", new = "ICU bed-days")
    icu = rbind(icu, data.table(Region = "All", "ICU bed-days" = sum(icu$`ICU bed-days`)))
    ## Invasive ventil invasive
    ventil = data[, sum(Bedinvasive.ventil), by = Region]
    setnames(ventil, old = "V1", new = "Invasive ventilation bed-days")
    ventil = rbind(ventil, data.table(Region = "All", "Invasive ventilation bed-days" = sum(ventil$`Invasive ventilation bed-days`)))
    
    ## Combine tables
    bedreq_byreg = Reduce(merge, list(hosp, icu, ventil))

    return(bedreq_byreg)
}


##     DT[, Infected := paste0(round(Infected.x, digits = 0),
##                             '-',
##                             round(Infected.y, digits = 0)
##                             )]

##     DT[, `Severe cases` := paste0(round(`Severe cases.x`, digits = 0),
##                                   '-',
##                                   round(`Severe cases.y`, digits = 0)
##                                   )]

##     DT[, ICU := paste0(round(ICU.x, digits = 0),
##                        '-',
##                        round(ICU.y, digits = 0)
##                        )]
    
##     DT[, Ventilation := paste0(round(Ventilation.x, digits = 0),
##                                '-',
##                                round(Ventilation.y, digits = 0)
##                                )]
    
##     DT[, Deaths := paste0(round(Deaths.x, digits = 0),
##                           '-',
##                           round(Deaths.y, digits = 0)
##                           )]

##     return(DT[, .(Region, Infected, `Severe cases`, ICU, Ventilation, Deaths)])
## }

