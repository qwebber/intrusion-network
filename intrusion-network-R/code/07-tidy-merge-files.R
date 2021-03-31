

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp',
          'RCurl')
lapply(libs, require, character.only = TRUE)


## load lifetime data
life <- fread("output/lifetime_clean.csv")

## load network metrics
metrics <- fread("output/metrics.csv")
metrics$gr_year <- as.factor(paste(metrics$grid, metrics$year, sep = "_"))


## load annual density
density <- fread("output/spring-density.csv")
density$gr_year <- as.factor(paste(density$grid, density$year, sep = "_"))

all <- merge(metrics, density[,c("V1", "year", "grid") := NULL], by = "gr_year")

merge(all, life[,c("sex", "byear", "squirrel_id")], by = "squirrel_id", fill = T)


mod1 <- lmer(outstrength ~ spr_density + (1|grid/squirrel_id), data = all)
summary(mod1)



