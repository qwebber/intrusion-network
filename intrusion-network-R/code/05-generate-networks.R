

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
edge_list[, Nowner := .N, by = c("owner", "grid", "year", "data")]

## number of obs per intruder
edge_list[, Nintruder := .N, by = c("intruder", "grid", "year", "data")]

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
metrics2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)]#[,c("gr_year") := NULL]
metrics2$id_gr_yr <- as.factor(paste(metrics2$squirrel_id, metrics2$gr_year, sep = "_"))



## generate metrics only using behavioural data
metrics_behav <- c()
edge_behav <- edge_list[data == "behaviour"]
edge_behav[, Nowner := .N, by = c("owner", "grid", "year", "data")]
## number of obs per intruder for behavioural data only
edge_behav[, Nintruder := .N, by = c("intruder", "grid", "year", "data")]

for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_behav[gr_year == k][, .N, by = c("owner", "intruder", "Nintruder")]
  
  ## generate territoriality index (intrusions/intrusions + total locs)
  df2 <- data.table(owner = df1$owner,
                    intruder = df1$intruder, 
                    TI = df1$N/(df1$N + df1$Nintruder))
  
  adj <- AdjacencyFromEdgelist(df2)
  
  diag(adj$adjacency) <- 0
  
  g <- graph_from_adjacency_matrix(adj$adjacency, weighted = TRUE, mode = c("directed")) 
  
  metrics_behav[[i]] <- data.table(outstrength_behav = graph.strength(g, mode = c("out")),
                             instrength_behav = graph.strength(g, mode = c("in")),
                             squirrel_id = adj$nodelist,
                             gr_year = k)
  
}

metrics_behav2 <- rbindlist(metrics_behav, fill = T)
metrics_behav2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)]#[,c("gr_year") := NULL]
metrics_behav2$id_gr_yr <- as.factor(paste(metrics_behav2$squirrel_id, metrics_behav2$gr_year, sep = "_"))


## generate metrics only using trapping data
metrics_trap <- c()
edge_trap <- edge_list[data == "trap"]
edge_trap[, Nowner := .N, by = c("owner", "grid", "year", "data")]
## number of obs per intruder for trapping data only
edge_trap[, Nintruder := .N, by = c("intruder", "grid", "year", "data")]

for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_trap[gr_year == k][, .N, by = c("owner", "intruder", "Nintruder")]
  
  ## generate territoriality index (intrusions/intrusions + total locs)
  df2 <- data.table(owner = df1$owner,
                    intruder = df1$intruder, 
                    TI = df1$N/(df1$N + df1$Nintruder))
  
  adj <- AdjacencyFromEdgelist(df2)
  
  diag(adj$adjacency) <- 0
  
  g <- graph_from_adjacency_matrix(adj$adjacency, weighted = TRUE, mode = c("directed")) 
  
  metrics_trap[[i]] <- data.table(outstrength_trap = graph.strength(g, mode = c("out")),
                                   instrength_trap = graph.strength(g, mode = c("in")),
                                   squirrel_id = adj$nodelist,
                                   gr_year = k)
  
}

metrics_trap2 <- rbindlist(metrics_trap, fill = T)
metrics_trap2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)]#[,c("gr_year") := NULL]
metrics_trap2$id_gr_yr <- as.factor(paste(metrics_trap2$squirrel_id, metrics_trap2$gr_year, sep = "_"))


metrics_all <- merge(metrics2, metrics_behav2[,c("outstrength_behav", "instrength_behav", "id_gr_yr")], 
                     by = "id_gr_yr", all = T)

metrics_all2 <- merge(metrics_all, metrics_trap2[,c("outstrength_trap", "instrength_trap", "id_gr_yr")], 
                      by = "id_gr_yr", all = T)

fake <- data.table(x = 0:1000,
                   y = 0:1000)

fake2 <- data.table(x = 0:5500,
                   y = 0:5500)

aa <- ggplot(metrics_all2) + 
  geom_point(aes(outstrength_trap, outstrength)) +
  geom_line(data = fake, aes(x,y), color = "red") +
  ggtitle("RED LINE IS 1:1 line") +
  ylab("Outstrength (all data)") +
  xlab("Outstrength (trapping data)") 
bb <- ggplot(metrics_all2) + 
  geom_point(aes(outstrength_trap, outstrength_behav)) +
  geom_line(data = fake, aes(x,y), color = "red") +
  xlab("Outstrength (trapping data)") +
  ylab("Outstrength (behavioural obs data)") 
cc <- ggplot(metrics_all2) + 
  geom_point(aes(outstrength, outstrength_behav)) +
  geom_line(data = fake2, aes(x,y), color = "red") +
  ylab("Outstrength (behavioural obs data)") +
  xlab("Outstrength (all data)") 
grid.arrange(aa,bb,cc, nrow = 1)

saveRDS(metrics_all2, "output/metrics.RDS")

