

### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp', 'dils')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))
yr <- fread("output/unique-grid-years.csv")

yr <- data.table(gr_year = unique(edge_list$gr_year))

## number of unique grid-years
n <- length(unique(yr$gr_year))
gr_year <- unique(yr$gr_year)

## number of obs per owner
edge_list[, Nowner := .N, by = c("owner", "grid", "year")]

## number of obs per intruder
edge_list[, Nintruder := .N, by = c("intruder", "grid", "year")]

## generate metrics
metrics <- c()
for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_list[gr_year == k][, .N, by = c("owner", "intruder", "Nintruder")]
 
  ## generate territoriality index (intrusions/intrusions + total locs)
  df2 <- data.table(owner = df1$owner,
                    intruder = df1$intruder, 
                    TI = df1$N/(df1$N + df1$Nintruder))
  
  adj <- AdjacencyFromEdgelist(df2)
  
  diag(adj$adjacency) <- 0
  
  g <- graph_from_adjacency_matrix(adj$adjacency, weighted = TRUE, mode = c("directed")) 
  
  metrics[[i]] <- data.table(outstrength = graph.strength(g, mode = c("out")),
                             instrength = graph.strength(g, mode = c("in")),
                             squirrel_id = adj$nodelist,
                             gr_year = k)

}

metrics2 <- rbindlist(metrics, fill = T)
metrics2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]



fwrite(metrics2, "output/metrics.csv")

