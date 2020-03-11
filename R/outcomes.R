#' Compute outcomes from modelOutput
#'
#' @param modelOutput output returned by a model run
#' @param severity_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
#' @param ICU_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
#' @param DaysHosp single value, corresponding to the length of stay in hospital for severe cases
#' @param DaysICU single value, corresponding to the length of stay in ICU for severe cases
#'
#' 
compute_outcomes <- function(modelOutput,
                             severity_risk,
                             ICU_risk,
                             ventil_risks,
                             DaysHosp,
                             DaysICU,
                             DaysVentil) {

    data = modelOutput[, .(Time, AgeGroup, Infected)]

    ## Merge tables
    outcome_table = merge(data,
                          severity_risk,
                          by = "AgeGroup")
    setnames(outcome_table, old = "risk", new = "severity_risk")
    outcome_table = merge(outcome_table,
                          ICU_risk,
                          by = "AgeGroup")
    setnames(outcome_table, old = "risk", new = "ICU_risk")

    ## Compute cases
    outcome_table[, severe := Infected*severity_risk]
    outcome_table[, non.severe := Infected-severe]
    outcome_table[, ICU := Infected*ICU_risk]
    outcome_table[, non.ICU := Infected-ICU]
    outcome_table[, overall.ventil  := ICU * ventil_risks$overall]
    outcome_table[, invasive.ventil := ICU * ventil_risks$invasive]
    outcome_table[, non.invasive.ventil := overall.ventil - invasive.ventil]

    ## Compute hospital-related information
    outcome_table[, BedHosp := sapply(Time, 
                                     function(parameter) 
                                         sum(severe[between(Time, parameter-DaysHosp+1, parameter)])),
                  by = c("AgeGroup")]
    outcome_table[, BedICU := sapply(Time, 
                                      function(parameter) 
                                          sum(ICU[between(Time, parameter-DaysICU+1, parameter)])),
                  by = c("AgeGroup")]
    outcome_table[, Bedinvasive.ventil := sapply(Time, 
                                     function(parameter) 
                                         sum(invasive.ventil[between(Time, parameter-DaysVentil+1, parameter)])),
                  by = c("AgeGroup")]
    
    return(outcome_table)
    
}

#' Render stacked bar chart and table by age distribution
#'
#' @param outcome_table the table returned by function compute_outcomes
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr %>%
outcome_render = function(outcome_table,
                          start_time,
                          end_time,
                          outcome = "severity") {

    
    data = outcome_table[Time >= start_time & Time <= end_time,]
    data[AgeGroup == "0-4", AgeGroup := "00-04"]
    data[AgeGroup == "5-9", AgeGroup := "05-09"]
    
    if (outcome == "severity") {
        ## Prepare data
        severe     = data[, sum(severe), by = AgeGroup]
        non_severe = data[, sum(non.severe), by = AgeGroup]
        plot_data = merge(severe, non_severe, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup","severe", "non.severe"))
        plot_data[, ":=" (severe = round(severe,0),
                          non.severe = round(non.severe,0))] 
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~severe,
                      type = 'bar',
                      name = 'Severe')
        fig = fig %>% add_trace(y = ~non.severe, name = 'Non-severe')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      severe = plot_data[,sum(severe)],
                                      non.severe = plot_data[,sum(non.severe)]))
    }
    else if (outcome == "ICU") {
        ## Prepare data
        ICU     = data[, sum(ICU), by = AgeGroup]
        non_ICU = data[, sum(non.ICU), by = AgeGroup]
        plot_data = merge(ICU, non_ICU, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup","ICU", "non.ICU"))
        plot_data[, ":=" (ICU = round(ICU,0),
                          non.ICU = round(non.ICU,0))] 
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~ICU,
                      type = 'bar',
                      name = 'ICU')
        fig = fig %>% add_trace(y = ~non.ICU, name = 'Non-ICU')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      ICU = plot_data[,sum(ICU)],
                                      non.ICU = plot_data[,sum(non.ICU)]))
    }
    else if (outcome == "ventilation") {
        ## Prepare data
        invasive     = data[, sum(invasive.ventil), by = AgeGroup]
        non_invasive = data[, sum(non.invasive.ventil), by = AgeGroup]
        non_ventil   = data[, sum(ICU) - sum(overall.ventil), by = AgeGroup]
        plot_data = merge(invasive, non_invasive, by = "AgeGroup")
        plot_data = merge(plot_data, non_ventil, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup", "invasive", "non.invasive", "non.ventil"))
        plot_data[, ":=" (invasive = round(invasive,0),
                          non.invasive = round(non.invasive,0),
                          non.ventil = round(non.ventil,0))] 
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~invasive,
                      type = 'bar',
                      name = 'Invasive')
        fig = fig %>% add_trace(y = ~non.invasive, name = 'Non-invasive')
        fig = fig %>% add_trace(y = ~non.ventil, name = 'Non-ventil')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      invasive = plot_data[,sum(invasive)],
                                      non.invasive = plot_data[,sum(non.invasive)],
                                      non.ventil = plot_data[,sum(non.ventil)]))
    }
    else if (outcome == "Infected") {
        plot_data = data[, sum(Infected), by = AgeGroup]
        setnames(plot_data, old = "V1", new = "Infected")
        plot_data[, Infected := round(Infected,0)] 
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~Infected,
                      type = 'bar',
                      name = 'New infected cases')
        fig = fig %>% layout(yaxis = list(title = 'Count'))
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      Infected = plot_data[,sum(Infected)]))
    }
    else if (outcome == "bedhosp") {
        plot_data = data[, .(AgeGroup,Number.hosp.beds = round(BedHosp,0))]
        plot_data <- plot_data[order(AgeGroup)]
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~Number.hosp.beds,
                      type = 'bar',
                      name = 'Number of hospital beds')
        fig = fig %>% layout(yaxis = list(title = 'Count'))
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      Number.hosp.beds = plot_data[,sum(Number.hosp.beds)]))
    }
    else if (outcome == "bedICU") {
        plot_data = data[, .(AgeGroup, Number.ICU.beds = round(BedICU,0))]
        plot_data <- plot_data[order(AgeGroup)]
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~Number.ICU.beds,
                      type = 'bar',
                      name = 'Number of ICU beds')
        fig = fig %>% layout(yaxis = list(title = 'Count'))
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      Number.ICU.beds = plot_data[,sum(Number.ICU.beds)]))
    }
    else if (outcome == "bedventil") {
        plot_data = data[, .(AgeGroup, Number.invasive.ventil = round(Bedinvasive.ventil,0))]
        plot_data <- plot_data[order(AgeGroup)]
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~Number.invasive.ventil,
                      type = 'bar',
                      name = 'Number of invasive ventilations')
        fig = fig %>% layout(yaxis = list(title = 'Count'))
        plot_data <- rbind(plot_data,
                           data.table(AgeGroup = "Total",
                                      Number.invasive.ventil = plot_data[,sum(Number.invasive.ventil)]))
    }
    
    return(list(plot = fig,
                table = plot_data))

}

outcome_render_instant_curve = function(outcome_table,
                                        instant_time,
                                        outcome = "bedhosp"){
  #browser()
  if (outcome == "bedhosp") {
    #browser()
    plot_data = outcome_table[, round(sum(BedHosp),0), by = "Time"]
    #plot_data = data[, .(AgeGroup,Number.hosp.beds = round(BedHosp,0))]
    #plot_data <- plot_data[order(AgeGroup)]
    fig = ggplot(plot_data, aes(x = Time, y = V1)) +
      geom_line() + 
      geom_vline(xintercept = instant_time, size = 1, color = "red") +
      theme_classic(16) + 
      scale_y_continuous(name = 'Number of hospital beds used per day') +
      annotate("text", x = instant_time + 5, y = 0.95 * max(plot_data$V1),
              label = paste0(plot_data[Time == instant_time,V1])) +
      theme(axis.title.x = element_blank()) 


  }
  else if (outcome == "bedICU") {
    plot_data = data[, .(AgeGroup, Number.ICU.beds = round(BedICU,0))]
    plot_data <- plot_data[order(AgeGroup)]
    fig = plot_ly(plot_data,
                  x = ~AgeGroup,
                  y = ~Number.ICU.beds,
                  type = 'bar',
                  name = 'Number of ICU beds')
    fig = fig %>% layout(yaxis = list(title = 'Count'))
    plot_data <- rbind(plot_data,
                       data.table(AgeGroup = "Total",
                                  Number.ICU.beds = plot_data[,sum(Number.ICU.beds)]))
  }
  else if (outcome == "bedventil") {
    plot_data = data[, .(AgeGroup, Number.invasive.ventil = round(Bedinvasive.ventil,0))]
    plot_data <- plot_data[order(AgeGroup)]
    fig = plot_ly(plot_data,
                  x = ~Time,
                  y = ~Number.invasive.ventil,
                  type = 'bar',
                  name = 'Number of invasive ventilations')
    fig = fig %>% layout(yaxis = list(title = 'Count'))
    plot_data <- rbind(plot_data,
                       data.table(AgeGroup = "Total",
                                  Number.invasive.ventil = plot_data[,sum(Number.invasive.ventil)]))
  }
  
  return(fig)
}
