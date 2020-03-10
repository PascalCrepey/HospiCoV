#' Get severity proportions by age
#'
#' @param modelOutput output returned by a model run
#' @param severity_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
severity_byage = function(modelOutput, severity_risk) {

    severity_table = merge(modelOutput[, .(Time, AgeGroup, Infected)],
                           severity_risk,
                           by = "AgeGroup")

    severity_table[, severe := Infected*risk]
    severity_table[, non.severe := Infected-severe]
        

    return(severity_table)
    
                   
}

#' Get ICU hospit proportions by age
#'
#' @param modelOutput output returned by a model run
#' @param ICU_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
ICU_byage = function(modelOutput, ICU_risk) {

    ICU_table = merge(modelOutput[, .(Time, AgeGroup, Infected)],
                      ICU_risk,
                      by = "AgeGroup")

    ICU_table[, severe := Infected*risk]
    ICU_table[, non.severe := Infected-severe]
        
    return(ICU_table)
                   
}



#' Render stacked bar chart for severity distribution
#'
#' @param outcome_table the table returned by function severity_byage
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr %>%
outcome_barchart = function(outcome_table, start_time, end_time, outcome = "severity") {

    data = outcome_table[Time >= start_time & Time <= end_time,]
    age_groups = unique(outcome_table$AgeGroup)
    severe     = data[, sum(severe), by = AgeGroup]
    non_severe = data[, sum(non.severe), by = AgeGroup]

    data = merge(severe, non_severe, by = "AgeGroup")
    setnames(data, c("AgeGroup","severe", "non.severe"))

    if (outcome == "severity") {
        fig = plot_ly(data,
                      x = ~AgeGroup,
                      y = ~severe,
                      type = 'bar',
                      name = 'Severe')
        fig = fig %>% add_trace(y = ~non.severe, name = 'Non-severe')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    }
    else if (outcome == "ICU") {
        fig = plot_ly(data,
                      x = ~AgeGroup,
                      y = ~severe,
                      type = 'bar',
                      name = 'ICU')
        fig = fig %>% add_trace(y = ~non.severe, name = 'Non-ICU')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    }
    
    
        
return(fig)

}
