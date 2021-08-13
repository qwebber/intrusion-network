


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf', 'spatsoc',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

df2 <- df[, median(locx), by = c("squirrel_id", "gr_year")]
df2$locy <- df[, median(locy), by = c("squirrel_id", "gr_year")]$V1 
setnames(df2, "V1", "locx")

df_nn <- edge_dist(df2, id = "squirrel_id", coords = c("locx", "locy"), 
                    timegroup = NULL, threshold = 10000, returnDist = T, 
                    splitBy = "gr_year")

df_nn$dyad <- as.factor(paste(df_nn$ID1, df_nn$ID2, df_nn$gr_year, sep = "_"))

## load edge list data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$grid, edge_list$year, sep = "_"))
yr <- fread("output/unique-grid-years.csv")

yr <- data.table(gr_year = unique(edge_list$gr_year))

## number of unique grid-years
n <- length(unique(yr$gr_year))
gr_year <- unique(yr$gr_year)

## number of obs per owner
edge_list[, Nowner := .N, by = c("owner", "grid", "year")]

## number of obs per intruder
edge_list[, Nintruder := .N, by = c("intruder", "grid", "year")]

edge_list <- edge_list[edge != 0]

## generate metrics
out2 <- c()
for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_list[gr_year == k][, .N, by = c("owner", "intruder", "Nintruder", "gr_year")]
  
    
  ## generate territoriality index (intrusions/intrusions + total locs)
  df2 <- data.table(owner = df1$owner,
                    intruder = df1$intruder, 
                    Nintruder = df1$Nintruder)
    
  adj <- AdjacencyFromEdgelist(df2)
    
  diag(adj$adjacency) <- 0
  
  adj2 <- as.matrix(adj$adjacency)
  colnames(adj2) <- adj$nodelist
  rownames(adj2) <- adj$nodelist
  
  out1 <- data.table(setNames(melt(adj2), c('owner', 'intruder', 'Nintruder')))
  out1$gr_year <- k  
  
  ## generate territoriality index (intrusions/intrusions + total locs)
  out2[[i]] <- out1
  
  
}

out2 <- rbindlist(out2, fill = T)
out2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)]
out2$dyad <- as.factor(paste(out2$owner, out2$intruder, out2$gr_year, sep = "_"))

aa <- merge(out2[, c("gr_year") := NULL], df_nn, by = "dyad")
aa[, c("dyad", "ID1", "ID2") := NULL]
aa$dyad <- as.factor(paste(aa$owner, aa$intruder, sep = "_"))

ggplot(aa, aes(distance, Nintruder)) +
  geom_point() +
  geom_smooth()
