

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf','igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
df <- fread("output/spatial-locs-2016.csv")
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

## drop all squirrels with <30 observations
df <- df[N > 30]

df$gr_year <- as.factor(paste(df$year, df$grid, sep = "_"))

## assign grid year
yr <- data.table(gr_year = as.factor(unique(df$gr_year)))

n <- length(unique(yr$gr_year))

## generate list of spatial points dataframes
out_mats <- c()
for(i in 1:n){ 
  
  df1 <- edge_list[[i]][, .N, by = c("owner", "intruder")]

  mat <- df1 %>%
          mutate_if(is.character, factor, levels = unique(df1$intruder)) %>%
          xtabs(N ~ owner + intruder, ., drop.unused.levels = F) 

  out_mats[[i]] <- mat
}

## name matrices
names(out_mats) <- yr$gr_year

metrics <- c()
for(i in 1:n){ 
  
  grph <- graph.adjacency(out_mats[[i]], weighted = T, mode = "directed")

  metrics[[i]] <- data.table(instrength = graph.strength(grph, mode = c("in")),
                             outstrength = graph.strength(grph, mode = c("out")),
                             evcent = evcent(grph)$vector,
                             between = betweenness(grph, v = V(grph), directed = TRUE,
                                           nobigint = TRUE, normalized = FALSE),
                             squirrel_id = names(degree(grph)),
                             obs = sum(out_mats[[i]]),
                             gr_year = names(out_mats)[i])

}


metrics2 <- rbindlist(metrics)
metrics2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

ggplot(metrics2) +
  geom_point(aes(outstrength/obs, instrength/obs, color = grid)) +
  xlab('number of intrusions') +
  ylab('number of times being intruded') +
  facet_wrap(~year)
