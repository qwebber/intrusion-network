

### Packages ----
libs <- c('data.table', 'igraph',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
yr <- fread("output/unique-grid-years.csv")



## remove juvenile points from mothers territory

ped <- tbl(con, "pedigree_clean") %>%
  collect()

setDT(ped)