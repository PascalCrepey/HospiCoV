library(openxlsx)
library(data.table)

## SEVERITY
## From Guan et al.
severity_risk = data.table(read.xlsx("./data-raw/risk-byage.xlsx", 
                                     sheet = "severity_risk"))

usethis::use_data(severity_risk, overwrite = TRUE)


## ICU
## From Guan et al.
ICU_risk = data.table(read.xlsx("./data-raw/risk-byage.xlsx", 
                                sheet = "ICU_risk"))

usethis::use_data(ICU_risk, overwrite = TRUE)


## Ventilation risk, WHEN ADMITTED IN ICU
## From Yang et al.
ventil_risks = data.table(read.xlsx("./data-raw/risk-byage.xlsx", 
                                    sheet = "ventil_risks"))

usethis::use_data(ventil_risks, overwrite = TRUE)

## Death risk, letality
## From Istituto Superiore si Sanita, Italie
death_risk = data.table(read.xlsx("./data-raw/risk-byage.xlsx", 
                                  sheet = "death_risk"))

usethis::use_data(death_risk, overwrite = TRUE)
