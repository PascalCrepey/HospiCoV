library(data.table)
library(openxlsx)
library(stringr)

#### Loading data ####
pop_town_age <- fread("./Data/population_allage_INSEE_2016.csv", dec = ",") # Last census in France in 2016
pop_town_age_arm <- fread("./Data/population_allage_INSEE_2016_arm.csv", dec = ",") # Arrondissement - Last census in France in 2016
pop_town_2017 <- fread("./Data/population_INSEE_2017.csv") # Use PMUN for town population in 2017 (population to use in 2020)
town_gps <- fread("./Data/town_GPS.csv") # GPS coordinates of each town

pop_town_age[, CODGEO := str_pad(CODGEO, 5, pad = "0")]
pop_town_age_arm[, CODGEO := str_pad(CODGEO, 5, pad = "0")]
pop_town_2017[, DEPCOM := str_pad(DEPCOM, 5, pad = "0")]
town_gps[, Code_commune_INSEE := str_pad(Code_commune_INSEE, 5, pad = "0")]
town_gps[, lat := as.numeric(unlist(strsplit(coordonnees_gps, ","))[1]),
         by = 1:town_gps[,.N]] # Latitude
town_gps[, lng := as.numeric(unlist(strsplit(coordonnees_gps, ","))[2]),
         by = 1:town_gps[,.N]] # Longitude

#### Data anagement on population ####
## Merge of pop_town_age and pop_town_age_arm to include arrondissement of Paris, Marseille and Lyon ##
pop_town_age <- pop_town_age[!CODGEO %in% c(13055, 69123, 75056), ] # Codes of Paris, Marseille and Lyon as a whole city
pop_town_age <- rbind(pop_town_age, 
                      pop_town_age_arm)
## Total number of persons per town in 2016 ##
pop_town_age[, POP_TOT2016 := rowSums(.SD),
             .SDcols = grep("SEXE", names(pop_town_age))]
## Merge of 2016 and 2017 data ##
pop_town <- merge(pop_town_age,
                  pop_town_2017[, .(DEPCOM, POP_TOT2017 = PMUN)],
                  by.x = "CODGEO",
                  by.y = "DEPCOM")
# Coefficient of population evolution #
pop_town[,EVO_COEF := POP_TOT2017/POP_TOT2016]
pop_town <- cbind(pop_town[, .(CODGEO, LIBGEO)],
                sweep(pop_town[,.SD, 
                               .SDcols = grep("SEXE", names(pop_town))],
                      1,
                      pop_town[,EVO_COEF], 
                      FUN = "*"))
pop_town[, POP_TOT2017 := rowSums(.SD),
         .SDcols = grep("SEXE", names(pop_town))]

#### Merge of GPS coordinates with population ####
pop_town <- unique(merge(pop_town, 
                town_gps[, .(Code_commune_INSEE, lat, lng)],
                by.x = "CODGEO",
                by.y = "Code_commune_INSEE",
                all.x = TRUE))
# Deletion of the town no longer existing #
pop_town <- pop_town[!is.na(lat),]

#### Save of the results ####
saveRDS(pop_town, file = "./Data/Population.rds")
