library(openxlsx)
library(data.table)

CaseTimeSeries <- data.table(read.xlsx("./data-raw/EpiDataFrance.xlsx", 
                                      sheet = "TimeSeries Cases", 
                                      detectDates = TRUE))
names(CaseTimeSeries) <- gsub(names(CaseTimeSeries), pattern = "\\.", 
                             replacement = " ")
colnames(CaseTimeSeries)[1] = "Date"
usethis::use_data(CaseTimeSeries, overwrite = TRUE)

longCTS = melt(CaseTimeSeries, id.vars = "Date", variable.name = "Region", value.name = "Cases")


#pre_infected = longCTS[Cases >= 1, .(Date = Date[1], preInfected = Cases[1]), by = "Region"]

pre_infected = longCTS[Date == as.Date("2020-03-10"), .(Date = Date[1], preInfected = Cases[1]), by = "Region"]

early_pre_infected = longCTS[Date == as.Date("2020-03-05"), .(Date = Date[1], preInfected = Cases[1]), by = "Region"]

usethis::use_data(pre_infected, overwrite = TRUE)
usethis::use_data(early_pre_infected, overwrite = TRUE)
