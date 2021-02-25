


### Packages ----
libs <- c('data.table', 'lubridate' ,'devtools',
          'dplyr')
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
  collect() %>% 
  filter(gr %in% c("KL")) %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) 
setDT(trp)

## add julian date, year, drop NAs
trp <-trp %>% 
  mutate(date=ymd(date),
         julian = yday(date),
         year = year(date),
         squirrel_id = as.factor(squirrel_id)) %>% 
  filter(year == 2016 | year == 2017,
         !is.na(squirrel_id),
         !is.na(locx),
         !is.na(locy),
         locx > -15,
         locx < 20,
         locy < 25,
         locy > -5) %>% 
  droplevels()

## pull relevant variables
trp <- trp[,c("squirrel_id", "locx", "locy", "gr",
       "date", "julian", "year")]

## assign column to indicate data are trapping
trp$data <- "trap"

## change column name "gr" to "grid" to match behaviour database
setnames(trp, "gr", "grid")

## pull census database
census <- tbl(con, "census") %>% 
  filter(gr %in% c ("KL"),
         census_date == "2016-05-15") %>% 
  collect() %>% 
  dplyr::select(squirrel_id)

## pull behaviour database
behaviour <- tbl(con, "behaviour") %>% 
  filter(grid %in% c("KL")) %>% 
  collect() %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) 
setDT(behaviour)

behaviour[, .N, by = "behaviour"]

## add julian date, year, drop NAs
behaviour<-behaviour %>% 
  mutate(date=ymd(date),
         julian = yday(date),
         year = year(date),
         mode=as.factor(mode),
         squirrel_id = as.factor(squirrel_id)) %>% 
  filter(!is.na(squirrel_id),
         !is.na(locx),
         !is.na(locy)) %>% 
  droplevels()

## filter to 2016 as an example year
behaviour_2016 <- behaviour %>% 
  filter (year == 2016 | year == 2017,
          locx > -15,
          locx < 20,
          locy < 25,
          locy > -5,
          #behaviour == 2, ## vocalizations 
          #behaviour == 3, 
          #detail == 1, ## animal material 
          squirrel_id %in% census$squirrel_id 
          #locx> 0,
          #locx < 12,
          #locy > 5,
          #locy < 15
  ) %>%
  droplevels()

## pull relevant variables
behaviour_2016 <- behaviour_2016[,c("squirrel_id", "locx", "locy", "grid",  
              "date", "julian", "year", "detail", "behaviour")]

behaviour_2016$data <- "behaviour"

df <- rbind(trp, behaviour_2016, fill = T)

fwrite(df, "output/spatial-locs-2016.csv")

