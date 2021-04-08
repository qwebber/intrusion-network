


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

## add min and max julian days for squirrel territories separately for owners and intruders
edge_list_owner <- edge_list[edge == 0][, minDay := min(julian), by = c("owner", "grid", "year")][, maxDay := max(julian), by = c("owner", "grid", "year")]
edge_list_intruder <- edge_list[edge == 1][, minDay := min(julian), by = c("owner", "grid", "year")][, maxDay := max(julian), by = c("owner", "grid", "year")]

edge_list <- rbind(edge_list_owner, edge_list_intruder)

## assign presence of squirrels on territories

## 1: presence of a squirrel within the lifetime of the territory (i.e. true intrusion)


sq12613 <- edge_list[owner == 5194]

ggplot(sq12613) +
  geom_jitter(aes(julian, year, color = owner), 
              height = 0.01, alpha = 0.25) + 
  geom_errorbar(aes(y=year, xmin=minDay, xmax=maxDay, color = owner), 
                width=0.2, size=0.1) +
  facet_wrap(~edge)

edge_list[, index := if(get(julian) > node_split) node_child_left 
          else index , by = seq_len(nrow(test))]
