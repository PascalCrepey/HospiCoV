########################
#### Contact Matrix ####
########################
library(data.table)
library(openxlsx)

#### Getting the data ####
## Contact data ##
# From Prem et al. 2017 #
# Extension of the last class (75-79) to a new class 80P corresponding to 80 years and over #
# Daily contacts #
contact_matrix <- as.matrix(read.xlsx("./data-raw/ContactMatrix.xlsx"))
rownames(contact_matrix) <- colnames(contact_matrix)

## Population data ##
population <- readRDS("./data-raw/Population.rds")


#### Aggregate population data to match contact matrix ####
# Reminder : SEXE1 = Men and SEXE2 = Women #
## Melt data ##
population_melt <- melt(population[, -c("POP_TOT2017","lat","lng")], 
                        id.vars = c("CODGEO","LIBGEO"))
population_melt <- population_melt[order(CODGEO,variable)]
population_melt[, sex := as.numeric(substr(variable,5,5))]
population_melt[, age := as.numeric(substr(variable,14,16))]

## Identification of age class ##
population_melt[age %in% 0:4, class := "04"]
population_melt[age %in% 5:9, class := "59"]
population_melt[age %in% 10:14, class := "1014"]
population_melt[age %in% 15:19, class := "1519"]
population_melt[age %in% 20:24, class := "2024"]
population_melt[age %in% 25:29, class := "2529"]
population_melt[age %in% 30:34, class := "3034"]
population_melt[age %in% 35:39, class := "3539"]
population_melt[age %in% 40:44, class := "4044"]
population_melt[age %in% 45:49, class := "4549"]
population_melt[age %in% 50:54, class := "5054"]
population_melt[age %in% 55:59, class := "5559"]
population_melt[age %in% 60:64, class := "6064"]
population_melt[age %in% 65:69, class := "6569"]
population_melt[age %in% 70:74, class := "7074"]
population_melt[age %in% 75:79, class := "7579"]
population_melt[age >= 80, class := "80P"]

## Dcast data ##
population_melt[, name := paste0("SEX", sex, "_AGE", class)]
population_contact <- dcast(population_melt, 
                          formula = CODGEO + LIBGEO ~ name, 
                          value.var = "value", 
                          fun.aggregate = sum)

## Merge with GPS coordinates ##
population_contact <- merge(population_contact,
                            population[, .(CODGEO, POP_TOT2017, lat, lng)],
                            by = "CODGEO")

#### Saving results ####
# saveRDS(list(contact_matrix,        
#              population_contact),
#         file = "./Data/Contacts.rds")

usethis::use_data(contact_matrix)
usethis::use_data(population_contact)
