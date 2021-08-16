

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp',
          'RCurl')
lapply(libs, require, character.only = TRUE)


## load database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL")) %>% 
  dplyr::select(squirrel_id, gr, sex, byear=byear, dam_id, bcert=bcert)

flastall <- setDT(collect(flastall))
flastall$squirrel_id <- as.factor(flastall$squirrel_id)

## load network metrics
metrics <- readRDS("output/metrics.RDS")
metrics$gr_year <- as.factor(paste(metrics$grid, metrics$year, sep = "_"))


## load annual density
density <- readRDS("output/auxilliary-data/spring-density.RDS")
density$gr_year <- as.factor(paste(density$grid, density$year, sep = "_"))

all <- merge(metrics, setDT(density)[,c("year", "grid") := NULL], by = "gr_year")

## merge metrics, density, and other life-history information
all <- merge(all, flastall, by = "squirrel_id", fill = T)

## calculate age
all$age <- as.integer(all$year) - as.integer(all$byear)
all$id_yr_gr <- as.factor(paste(all$squirrel_id, all$grid, all$year, sep = "_"))


## load territory size data
terr <- readRDS("output/territory-area.RDS")
terr$id_yr_gr <- as.factor(paste(terr$squirrel_id, terr$grid, terr$year, sep = "_"))

all2 <- merge(all, terr[,c("area_m2", "id_yr_gr")], by = "id_yr_gr")

## load number of locs
locs <- fread("output/number_locs.csv")
locs$id_yr_gr <- as.factor(paste(locs$squirrel_id, locs$gr_year, sep = "_"))

## merge number of locs with main dataset
all3 <- merge(all2, locs[,c("N","id_yr_gr")], by = "id_yr_gr")

## add mast fixed effect
all3[, mast := (year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019)]
all3$mast[all3$mast == TRUE] <- "mast"
all3$mast[all3$mast == FALSE] <- "nomast"


saveRDS(all3, "output/final-df.RDS")

