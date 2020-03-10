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
                       "risk" = severity_risk)
                   
}
