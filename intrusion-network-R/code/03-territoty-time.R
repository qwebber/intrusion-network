


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

## add min and max julian days for squirrel territories
edge_list[, minDay := min(julian), by = c("owner", "grid", "year", "edge")]
edge_list[, maxDay := max(julian), by = c("owner", "grid", "year", "edge")]

## assign presence of squirrels on territories

## 1: presence of a squirrel within the lifetime of the territory (i.e. true intrusion)

sq12613 <- edge_list[owner == 12613]

ggplot(sq12613) +
  geom_jitter(aes(julian, year, color = edge), 
              height = 0.01, alpha = 0.25) + 
  geom_errorbar(aes(y=year, xmin=minDay, xmax=maxDay, color = edge), 
                width=0.2, size=0.1) +
  facet_wrap(~edge)

edge_list[, index := if(get(julian) > node_split) node_child_left 
          else index , by = seq_len(nrow(test))]
