library(openxlsx)
library(data.table)

CaseTimeSeries <- data.table(read.xlsx("./data-raw/EpiDataFrance.xlsx", 
                                      sheet = "TimeSeries Cases", 
                                      detectDates = TRUE))
names(CaseTimeSeries) <- gsub(names(CaseTimeSeries), pattern = "\\.", 
                             replacement = " ")
usethis::use_data(CaseTimeSeries, overwrite = TRUE)
