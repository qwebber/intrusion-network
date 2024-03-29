


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load home-made functions
source("functions/get_intersection.R")
source("functions/get_edgelist.R")

## load and merge polygons

## save SPDF
out_spdf <- readRDS("output/edge-list-inputs/spdf.RDS")

out_polygon90s <- readRDS("output/edge-list-inputs/polygons-1996-99.RDS")
out_polygon05 <- readRDS("output/edge-list-inputs/polygons-2000-2005.RDS")
out_polygon10 <- readRDS("output/edge-list-inputs/polygons-2006-2010.RDS")
out_polygon15 <- readRDS("output/edge-list-inputs/polygons-2011-2015.RDS")
out_polygon20 <- readRDS("output/edge-list-inputs/polygons-2016-2020.RDS")


polygons_all <- c(out_polygon90s, 
                      out_polygon05,
                      out_polygon10,
                      out_polygon15,
                      out_polygon20)
  
saveRDS(polygons_all, "output/edge-list-inputs/polygons-all.RDS")

yr <- fread("output/unique-grid-years.csv")

## generate territory size ##
area <- c()

for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(polygons_all[[i]]$id_polygons, polygons_all[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area[[i]] <- ar
  
}

## convert from list to data.table
area <- rbindlist(area)

area[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]

ggplot(area) +
  geom_jitter(aes(grid, area_m2, color = year)) +
  theme(legend.position = 'none') +
  facet_wrap(~year)

# export df of area
saveRDS(area, "output/territory-area.RDS")

# Intersection between polygon and points ---------------------------------
intersect_out <- get_intersection(poly = polygons_all, 
                                  spdf = out_spdf,
                                  n = length(yr$gr_year))

## name lists within intersect_out
names(intersect_out) <- yr$gr_year

## save intersection file
saveRDS(intersect_out, "output/intersect-pt-poly.RDS")

# Intersection between polygons 
intersect_out2 <- get_intersection(poly = polygons_all, 
                                  spdf = polygons_all,
                                  n = length(yr$gr_year))
names(intersect_out2) <- yr$gr_year

# Generate edge list based on polygon-point overlap ---------------------------------
edge_out <- get_edgelist(df = intersect_out, 
                         n = yr$gr_year)

edge_list <- rbindlist(edge_out)
setnames(edge_list, "year", "gr_year")

# Remove mom-offspring dyads ---------------------------------

## load lifetime data
life <- readRDS("output/auxilliary-data/lifetime-clean.RDS")

## add column for unique dyad ID of moms and offspring
life$dyad <- as.factor(paste(life$squirrel_id, life$dam_id, sep = "_"))
life$mom <- "yes"

## add column for unique dyad ID
edge_list$dyad <- as.factor(paste(edge_list$owner, edge_list$intruder, sep = "_"))


## merge to form file of just moms and their offspring
mom_offspring <- merge(edge_list, life[,c("dyad", "mom")], by = "dyad")
length(unique(mom_offspring$dyad))

## generate new edgelist file without moms and their offspring
edge_list <- edge_list %>% 
  filter(!dyad %in% mom_offspring$dyad)

## summary of observations that occurred on own territory vs. on a different territory
edge_list[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]

## look at discrepancy in number of intrusions through time 
ggplot(edge_list[, .N, by = c("grid", "year")]) + 
  geom_point(aes(year, N, color = grid))

## export edge list
saveRDS(edge_list, "output/edge_list.RDS")
