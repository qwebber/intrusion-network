


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
              "date", "julian", "year", "detail", "behaviour")]

behaviour_all$data <- "behaviour"

df <- rbind(trp, behaviour_all, fill = T)

## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

df$gr_year <- as.factor(paste(df$grid, df$year, sep = "_"))

## compare data subsets with minimum 15 and minimum 30 observations
df2 <- df[N > 15]
df3 <- df[N > 30]
ids15 <- df2[, uniqueN(squirrel_id), by = c("gr_year")]
ids30 <- df3[, uniqueN(squirrel_id), by = c("gr_year")]
df_obs <- merge(ids15, ids30, by = "gr_year", all = T)
setnames(df_obs, c("V1.x", "V1.y"), c("unique_ids_15_obs", "unique_ids_30_obs"))

## drop all squirrels with <15 observations and separate file with >30 observations
df15 <- df[N > 15]
df30 <- df[N > 30]

## drop instances where >10 observatioHns in a day
df15[, rowDay := seq_len(.N), by = c("squirrel_id","julian","grid", "year")]
df30[, rowDay := seq_len(.N), by = c("squirrel_id","julian","grid", "year")]

## remove any observations more than 30 in a day
df15 <- df15[rowDay < 31]
df30 <- df30[rowDay < 31]

## check number of IDs per grid-year
df15[, NID := uniqueN(squirrel_id), by = c("grid", "year")]
df30[, NID := uniqueN(squirrel_id), by = c("grid", "year")]

## remove grid-years with <10 unique squirrels
df15 <- df15[NID > 11]
df30 <- df30[NID > 11]

## number of unique years
length(unique(df15$year))
length(unique(df30$year))

## number of trappingevents/behavioural observations
df15[, .N, by = c("data")]
df30[, .N, by = c("data")]

## order dataframe by grid and year
df15 <- setDT(ddply(df15, c('year', 'grid')))
df30 <- setDT(ddply(df30, c('year', 'grid')))

fwrite(df15, "output/spatial-locs-15.csv")
fwrite(df30, "output/spatial-locs-30.csv")

