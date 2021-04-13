

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp',
          'RCurl')
lapply(libs, require, character.only = TRUE)


## load lifetime data
life <- readRDS("output/auxilliary-data/lifetime-clean.RDS")

## load network metrics
metrics <- fread("output/metrics.csv")
metrics$gr_year <- as.factor(paste(metrics$grid, metrics$year, sep = "_"))


## load annual density
density <- readRDS("output/auxilliary-data/spring-density.RDS")
density$gr_year <- as.factor(paste(density$grid, density$year, sep = "_"))

all <- merge(metrics, density[,c("V1", "year", "grid") := NULL], by = "gr_year")

#merge(all, life[,c("sex", "byear", "squirrel_id")], by = "squirrel_id", fill = T)

fwrite(all, "output/final-df.csv")
