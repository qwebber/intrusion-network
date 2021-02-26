

### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
yr <- fread("output/unique-grid-years.csv")

## number of unique grid-years
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
  
  out_mats_in <- out_mats
  out_mats_out <- out_mats
  
  out_mats_in[[i]][upper.tri(out_mats_in[[i]])] <- 0
  out_mats_out[[i]][lower.tri(out_mats_out[[i]])] <- 0
  
  grph_out <- graph.adjacency(out_mats_out[[i]], weighted = T)
  grph_in <- graph.adjacency(out_mats_in[[i]], weighted = T)
  
  metrics[[i]] <- data.table(outstrength = graph.strength(grph_out, mode = c("out")),
                             instrength = graph.strength(grph_in, mode = c("in")),
                             outevcent = evcent(grph_out)$vector,
                             inevcent = evcent(grph_in)$vector,
                             outbetween = betweenness(grph_out, v = V(grph_out), directed = TRUE,
                                           nobigint = TRUE, normalized = FALSE),
                             inbetween = betweenness(grph_in, v = V(grph_in), directed = TRUE,
                                                      nobigint = TRUE, normalized = FALSE),
                             squirrel_id = names(degree(grph_out)),
                             squirrel_id_check = names(degree(grph_in)),
                             obs_out = sum(out_mats_out[[i]]),
                             obs_in = sum(out_mats_in[[i]]),
                             gr_year = names(out_mats)[i])

}


metrics2 <- rbindlist(metrics, fill = T)
metrics2[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

fwrite(metrics2, "output/metrics.csv")

