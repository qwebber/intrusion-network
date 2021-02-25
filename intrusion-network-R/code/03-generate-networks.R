

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf','igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")

KL_2016 <- edge_list[[1]][, .N, by = c("owner", "intruder")]

mat <- KL_2016 %>%
          mutate_if(is.character, factor, levels = unique(KL_2016$intruder)) %>%
          xtabs(N ~ owner + intruder, ., drop.unused.levels = F) 


grph <- graph.adjacency(mat, weighted = T, mode = "directed")

metrics <- data.table(instrength = graph.strength(grph, mode = c("in")),
                      outstrength = graph.strength(grph, mode = c("out")),
                      incent = betweenness(grph, directed = T),
                      squirrel_id = names(degree(grph)))

ggplot(metrics) +
  geom_point(aes(instrength, outstrength))
