

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf','igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)



grph <- graph_from_edgelist(as.matrix(edge_list), directed=T)


graph.strength(grph, mode = c("out"))