


### Packages ----
libs <- c('data.table', 'devtools')
lapply(libs, require, character.only = TRUE)

devtools::install_github("KluaneRedSquirrelProject/krsp")

library(krsp)

con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)



## Pull trapping data
trp <- tbl(con, "trapping") %>%
  collect(trapping)
setDT(trp)

## extract year from date
trp[, year := year(date)]
trp[, JDate := yday(date)]

trp$locXnum <- loc_to_numeric(trp$locx)
trp$locYnum <- loc_to_numeric(trp$locy)

fwrite(trp, "output/trp.csv")

