


### Packages ----
libs <- c('data.table', 'lubridate' ,'devtools',
          'dplyr', 'plyr')
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
  filter(gr %in% c("KL","SU")) %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) 
setDT(trp)

## add julian date, year, drop NAs
trp <-trp %>% 
  mutate(date=ymd(date),
         julian = yday(date),
         year = year(date),
         squirrel_id = as.factor(squirrel_id)) %>% 
  filter(year == 2012 | year == 2013 | year == 2014 | 
           year == 2015 | year == 2016 | year == 2017,
         !is.na(squirrel_id),
         !is.na(locx),
         !is.na(locy),
         julian > 74, ## only include trapping between March 15 and Sept 1
         julian < 244,
         locx > -15,
         locx < 20,
         locy < 25,
         locy > -10) %>% 
  droplevels()

## pull relevant variables
trp <- trp[,c("squirrel_id", "locx", "locy", "gr",
       "date", "julian", "year", "rattle")]

## assign column to indicate data are trapping
trp$data <- "trap"

## change column name "gr" to "grid" to match behaviour database
setnames(trp, "gr", "grid")

## pull census database
census <- tbl(con, "census") %>% 
  filter(gr %in% c("KL", "SU")) %>% 
  collect() %>% 
  dplyr::select(squirrel_id)

## pull behaviour database
behaviour <- tbl(con, "behaviour") %>% 
  filter(grid %in% c("KL", "SU")) %>% 
  collect() %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) 
setDT(behaviour)

behaviour[, .N, by = "behaviour"]

## add julian date, year, drop NAs
behaviour <- behaviour %>% 
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
behaviour_all <- behaviour %>% 
  filter (#year == 2012 | year == 2013 | year == 2014 | 
          #  year == 2015 | year == 2016 | year == 2017,
          julian > 74, ## only include observations between March 15 and Sept 1
          julian < 244,
          locx > -15,
          locx < 20,
          locy < 25,
          locy > -10,
          #behaviour == 2, ## vocalizations 
          #behaviour == 3, 
          #detail == 1, ## animal material 
          squirrel_id %in% census$squirrel_id 
  ) %>%
  droplevels()

## pull relevant variables
behaviour_all <- behaviour_all[,c("squirrel_id", "locx", "locy", "grid",  
              "date", "julian", "year", "detail", "behaviour")]

behaviour_all$data <- "behaviour"

df <- rbind(trp, behaviour_all, fill = T)

## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

## drop all squirrels with <30 observations
df <- df[N > 30]

## drop instances where >10 observations in a day
df[, rowDay := seq_len(.N), by = c("squirrel_id","julian","grid", "year")]

df <- df[rowDay < 31]

## order dataframe by grid and year
df <- setDT(ddply(df, c('year', 'grid')))

fwrite(df, "output/spatial-locs.csv")

