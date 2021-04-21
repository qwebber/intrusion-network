

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

## load network metrics
metrics <- fread("output/metrics.csv")
metrics$gr_year <- as.factor(paste(metrics$grid, metrics$year, sep = "_"))


## load annual density
density <- readRDS("output/auxilliary-data/spring-density.RDS")
density$gr_year <- as.factor(paste(density$grid, density$year, sep = "_"))

all <- merge(metrics, setDT(density)[,c("year", "grid") := NULL], by = "gr_year")

## merge metrics, density, and other life-history information
all <- merge(all, flastall, by = "squirrel_id", fill = T)

## calculate age
all$age <- all$year - all$byear

saveRDS(all, "output/final-df.RDS")

