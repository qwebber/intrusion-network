


### Packages ----
libs <- c('data.table', 'lubridate' ,'devtools',
          'dplyr', 'plyr')
lapply(libs, require, character.only = TRUE)

devtools::install_github("KluaneRedSquirrelProject/krsp")

select = dplyr::select #necessary as MASS also has a select function

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
  filter(
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
sq <- tbl(con, "squirrel") %>% 
  filter(gr %in% c("KL", "SU")) %>% 
  collect() %>% 
  select(id)

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
         !is.na(locy)
         ) %>% 
  droplevels()

## filter to 2016 as an example year
behaviour_all <- behaviour %>% 
  filter (julian > 74, ## only include observations between March 15 and Sept 1
          julian < 244,
          locx > -15,
          locx < 20,
          locy < 25,
          locy > -10) %>%
  droplevels()

## pull relevant variables
behaviour_all <- behaviour_all[,c("squirrel_id", "locx", "locy", "grid",  
              "date", "julian", "year", "detail", "behaviour", "mode")]

behaviour_all$data <- "behaviour"

## remove modes 0, 6, and 7
behaviour_all <- behaviour_all[mode != 0 & mode != 6 & mode != 7]


df <- rbind(trp, behaviour_all, fill = T)

## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

df$gr_year <- as.factor(paste(df$grid, df$year, sep = "_"))

## drop individuals with fewer than 20 obs per year
df2 <- df[N > 21]

## assing observation number per id/day/grid/year
df2[, rowDay := seq_len(.N), by = c("squirrel_id","julian","grid", "year")]
df2$id_gr_yr_day <- as.factor(paste(df2$squirrel_id, df2$grid,
                                    df2$year, df2$julian, df2$rowDay, sep = "_"))



## split to assign rows to either be IDs with > 30 or <30 locs per day
under30 <- df2[, if (.N < 31) .SD, by = c("squirrel_id","julian","grid", "year")]
over30 <- df2[, if (.N > 30) .SD, by = c("squirrel_id","julian","grid", "year")]
over30$rowDay2 <- "over"
over30$id_gr_yr_day <- as.factor(paste(over30$squirrel_id, over30$grid,
                                       over30$year, over30$julian, over30$rowDay, sep = "_"))

## randomly select 30 locs per id/day/grid/year
over30_2 <- over30[, sample(rowDay, 30), by = c("squirrel_id","julian","grid", "year")]
setnames(over30_2,  "V1", "rowDay")
over30_2$id_gr_yr_day <- as.factor(paste(over30_2$squirrel_id, over30_2$grid,
                                         over30_2$year, over30_2$julian, over30_2$rowDay, sep = "_"))

## merge data from original DT back with >30 DT subset
over30_3 <- merge(df2, 
                  over30_2[, c("squirrel_id","julian","grid", "year", "rowDay") := NULL], 
                  by = "id_gr_yr_day")


## merge DT < 30 locs/day with DT > randomly sampled 30 locs/day 
df3 <- rbind(under30, over30_3)

## check number of IDs per grid-year
df3[, NID := uniqueN(squirrel_id), by = c("grid", "year")]

## check unique squirrels per year
df3[, uniqueN(squirrel_id), by = c("grid", "year")]

## remove grid-years with <10 unique squirrels
df3 <- df3[NID > 11]

## number of trappingevents/behavioural observations
df3[, .N, by = c("data")]

## order dataframe by grid and year
df3 <- setDT(ddply(df3, c('year', 'grid')))

## convert locx and locy to metres
df3$locx <- df3$locx*30
df3$locy <- df3$locy*30

#### assign a jitter to all locx and locy

## for trappign data use 15m jitter
dfTrap <- df3[data == "trap"]
dfTrap$locx <- dfTrap$locx + sample(1:15, size = length(dfTrap$locx), replace = T)
dfTrap$locy <- dfTrap$locy + sample(1:15, size = length(dfTrap$locy), replace = T)

## for behavioural data use 3m jitter
dfBehav <- df3[data == "behaviour"]
dfBehav$locx <- dfBehav$locx + sample(1:3, size = length(dfBehav$locx), replace = T)
dfBehav$locy <- dfBehav$locy + sample(1:3, size = length(dfBehav$locy), replace = T)

df3 <- rbind(dfTrap, dfBehav)

saveRDS(df3, "output/spatial-locs.RDS")

