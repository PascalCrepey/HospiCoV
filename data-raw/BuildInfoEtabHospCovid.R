library(data.table)
library(stringi)
library(stringr)
library(openxlsx)

info_etab = fread("data-raw/info_etab.csv")
info_etab[ , RS := stri_enc_toascii(RS)]
info_etab[ , address := stri_enc_toascii(address)]
info_etab[ , cat := stri_enc_toascii(cat)]
usethis::use_data(info_etab, overwrite = TRUE)


hospCovid = data.table(read.xlsx("data-raw/Hopitaux_covid.xlsx"))
hospCovid[, FINESS_GEO := as.character(FINESS_GEO)]
hospCovid[, FINESS_GEO := str_pad(FINESS_GEO, 9 , pad = "0")]
hospCovid <- hospCovid[!is.na(Ligne),]
usethis::use_data(hospCovid, overwrite = TRUE)
