


### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp', 'dils')
lapply(libs, require, character.only = TRUE)

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
