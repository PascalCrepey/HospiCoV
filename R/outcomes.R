#' Compute outcomes from modelOutput
#'
#' @param modelOutput output returned by a model run
#' @param severity_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
#' @param ICU_risk vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
#'
#' 
compute_outcomes <- function(modelOutput,
                             severity_risk,
                             ICU_risk,
                             ventil_risks) {

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

    return(outcome_table)
    
}


#' Render stacked bar chart by age distribution
#'
#' @param outcome_table the table returned by function compute_outcomes
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr %>%
outcome_barchart = function(outcome_table,
                            start_time,
                            end_time,
                            outcome = "severity") {

    
    data = outcome_table[Time >= start_time & Time <= end_time,]
    age_groups = unique(outcome_table$AgeGroup)
    
    if (outcome == "severity") {
        ## Prepare data
        severe     = data[, sum(severe), by = AgeGroup]
        non_severe = data[, sum(non.severe), by = AgeGroup]
        plot_data = merge(severe, non_severe, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup","severe", "non.severe"))
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~severe,
                      type = 'bar',
                      name = 'Severe')
        fig = fig %>% add_trace(y = ~non.severe, name = 'Non-severe')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    }
    else if (outcome == "ICU") {
        ## Prepare data
        ICU     = data[, sum(ICU), by = AgeGroup]
        non_ICU = data[, sum(non.ICU), by = AgeGroup]
        plot_data = merge(ICU, non_ICU, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup","ICU", "non.ICU"))
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~ICU,
                      type = 'bar',
                      name = 'ICU')
        fig = fig %>% add_trace(y = ~non.ICU, name = 'Non-ICU')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    }
    else if (outcome == "ventilation") {
        ## Prepare data
        invasive     = data[, sum(invasive.ventil), by = AgeGroup]
        non_invasive = data[, sum(non.invasive.ventil), by = AgeGroup]
        non_ventil   = data[, sum(ICU) - sum(overall.ventil), by = AgeGroup]
        plot_data = merge(invasive, non_invasive, by = "AgeGroup")
        plot_data = merge(plot_data, non_ventil, by = "AgeGroup")
        setnames(plot_data, c("AgeGroup", "invasive", "non.invasive", "non.ventil"))
        ## Plot
        fig = plot_ly(plot_data,
                      x = ~AgeGroup,
                      y = ~invasive,
                      type = 'bar',
                      name = 'Invasive')
        fig = fig %>% add_trace(y = ~non.invasive, name = 'Non-invasive')
        fig = fig %>% add_trace(y = ~non.ventil, name = 'Non-ventil')
        fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    }        

    return(fig)

}
