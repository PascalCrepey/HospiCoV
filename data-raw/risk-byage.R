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

## SEVERITY
## From Guan et al.
risks = c(0.11,
          0.11,
          0.11,
          0.12,
          0.12,
          0.12,
          0.12,
          0.12,
          0.12,
          0.12,
          0.17,
          0.17,
          0.17,
          0.29,
          0.29,
          0.29,
          0.29)
          
severity_risk = data.table("AgeGroup" = age_groups,
                            "risk" = risks)


usethis::use_data(severity_risk, overwrite = TRUE)


## ICU
## From Guan et al.
ICU_risks = c(0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.02,
              0.07,
              0.07,
              0.07,
              0.21,
              0.21,
              0.21,
              0.21)

ICU_risk = data.table("AgeGroup" = age_groups,
                      "risk" = ICU_risks)

usethis::use_data(ICU_risk, overwrite = TRUE)


## Ventilation risk, WHEN ADMITTED IN ICU
## From Yang et al.

ventil_risks = data.table("overall"  = 0.711,
                          "invasive" = 0.423)

usethis::use_data(ventil_risks, overwrite = TRUE)
