#' Get severity proportions by age
#'
#' @param modelOutput output returned by a model run
#' @param risk_byage vector of dimension 17, corresponding to the risk (proportion) of being a severe case, for each age group
severity_byage = function(modelOutput, risk_byage) {
    age_groups = c("0-4",
                   "5-9",
                   "10-14",
                   "15-19",
                   "20-24",
                   "25-29",
                   "30-34",
                   "35-39",
                   "40-44",
                   "45-49",
                   "50-54",
                   "55-59",
                   "60-64",
                   "65-69",
                   "70-74",
                   "75-79",
                   "80P")

    risks = data.table("AgeGroup" = age_groups,
                       "risk" = risk_byage)
    

    severity_table = merge(modelOutput[, .(Time, AgeGroup, Infected)],
                           risks,
                           by = "AgeGroup")

    severity_table[, severe := Infected*risk]
    severity_table[, non.severe := Infected-severe]
        

    return(severity_table)
    
                   
}

#' Render stacked bar chart for severity distribution
#'
#' @param severity_table the table returned by function severity_byage
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr %>%
severity_barchart = function(severity_table, start_time, end_time) {
    

    fig = plot_ly(severity_table[Time >= start_time & Time <= end_time,],
                  x = ~AgeGroup,
                  y = ~severe,
                  type = 'bar',
                  name = 'Severe')
    fig = fig %>% add_trace(y = ~non.severe, name = 'Non-severe')
    fig = fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
return(fig)

}
