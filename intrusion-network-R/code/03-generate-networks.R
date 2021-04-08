

### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))
yr <- fread("output/unique-grid-years.csv")

## number of unique grid-years
n <- length(unique(yr$gr_year))
gr_year <- unique(yr$gr_year)

## generate metrics
metrics <- c()
for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_list[gr_year == k][, .N, by = c("owner", "intruder")]
  
  adj <- AdjacencyFromEdgelist(df1)
  
  diag(adj$adjacency) <- 0
  
  g <- graph.adjacency(adj$adjacency) 
  
  metrics[[i]] <- data.table(outstrength = graph.strength(g, mode = c("out")),
                             instrength = graph.strength(g, mode = c("in")),
                             squirrel_id = adj$nodelist,
                             gr_year = k)

}

metrics2 <- rbindlist(metrics, fill = T)
metrics2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

fwrite(metrics2, "output/metrics.csv")

