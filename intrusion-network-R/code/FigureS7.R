
### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

ggplot(edge_list[, .N, by = c("julian", "year", "grid", "gr_year")],
       aes(julian, N)) +
  geom_point(aes(color = grid)) +
  geom_smooth(method = "loess") +
  facet_wrap(~year, scale = "free")
