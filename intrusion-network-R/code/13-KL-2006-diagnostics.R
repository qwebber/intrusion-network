


### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp', 'dils')
lapply(libs, require, character.only = TRUE)

### load raw data 
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

## check to see temporal changes in data collection
ggplot(df[year == 2005 | year == 2006 | year == 2007][, .N, by = c("squirrel_id","julian", "year")]) +
  geom_point(aes(julian, N, color = as.factor(year))) + 
  theme(legend.position = 'none') +
  facet_wrap(~year)

## check to see temporal changes in trapping vs. behavioural data collection
ggplot(df[, .N, by = c("data", "julian", "year")]) +
  geom_point(aes(julian, N, color = data))  +
  facet_wrap(~year)
## RESULT: 2006 has a large number of behavioural observations in the middle of the summer, 
## but not any more than 2014, 2016, or 2019.

## subset to KL_2006
df06 <- df[gr_year == "KL_2006"]

df06sub <- df06[squirrel_id == 10121 |
                  squirrel_id == 10183 |
                  squirrel_id == 7124 | 
                  squirrel_id == 10317 |
                  squirrel_id == 7092]

ggplot(df06sub) +
   geom_point(aes(locx/30, locy/30, color = squirrel_id)) +
   ylim(-2, 22) + xlim(-12, 22) +
   facet_wrap(~data)

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

edge_list06 <- edge_list[gr_year == "2006_KL"]

edge_list06[, .N, by = c("julian", "intruder_sex")]

ggplot(edge_list06[, .N, by = c("julian", "intruder_sex")]) +
  geom_point(aes(julian, N)) +
  facet_wrap(~intruder_sex)

edge_list06[, .N, by = c("intruder", "intruder_sex")]

ggplot(edge_list06[julian == 150]) +
  geom_jitter(aes(locx/30, locy/30, color = intruder), 
              alpha = 0.25, 
              height = 0.2, width = 0.2) +
  theme(legend.position = 'none')

aa <- edge_list06[julian == 150]
