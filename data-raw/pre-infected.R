x = unique(hospCovid$Region)
pre_infected = data.table(Region = x,
                          preInfected = 40)

usethis::use_data(pre_infected, overwrite = TRUE)
