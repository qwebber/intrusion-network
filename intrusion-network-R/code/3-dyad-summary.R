

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load pedigree data
ped <- tbl(con, "pedigree_clean") %>%
  collect()
setDT(ped)

## remove NAs from dam_id column
ped <- ped[!is.na(ped$dam_id)]

## add column for unique dyad ID of moms and offspring
ped$dyad <- as.factor(paste(ped$squirrel_id, ped$dam_id, sep = "_"))
ped$mom <- "yes"

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list <- rbindlist(edge_list)

## add column for unique dyad ID
edge_list$dyad <- as.factor(paste(edge_list$owner, edge_list$intruder, sep = "_"))

## merge to form file of just moms and their offspring
mom_offspring <- merge(edge_list, ped[,c("dyad", "mom")], by = "dyad")

## generate new edgelist file without moms and their offspring
edge_list <- edge_list %>% 
                filter(!dyad %in% mom_offspring$dyad)


df <- fread("output/spatial-locs.csv")

df[, .N, by = c("year", "grid")]

