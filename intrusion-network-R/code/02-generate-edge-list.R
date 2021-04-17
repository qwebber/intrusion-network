

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- fread("output/spatial-locs-15.csv")
df$squirrel_id <- as.character(df$squirrel_id)

## prj
prj <- '+init=epsg:26911'

## load home-made functions
source("functions/get_spdf.R")
source("functions/get_polygon.R")
source("functions/get_intersection.R")
source("functions/get_edgelist.R")

## generate list of grid-year combinations
yr <- data.table(gr_year = as.factor(unique(df$gr_year)))
fwrite(yr, "output/unique-grid-years.csv")

# Generate spdf points ---------------------------------
out_spdf <- get_spdf(df = df[,c("julian","locx", "locy", "squirrel_id", "gr_year")], 
               n = yr$gr_year)

## save SPDF
saveRDS(out_spdf, "output/edge_list_data/spdf.RDS")

## generate list of kernels

## parameters for kernel
params = c(grid = 400, extent = 7)

# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(df = df, 
                            n = yr$gr_year,
                            in.percent = 50,
                            params = params)

saveRDS(out_polygon, "output/edge_list_data/polygons.RDS")

## output area
area <- c()
for(i in 1:length(yr$gr_year)){ 
ar <- data.table(out_polygon[[i]]$id_polygons, out_polygon[[i]]$area)
ar$gr_year <- yr$gr_year[i]
setnames(ar, c("V1", "V2"), c("squirrel_id", "area_ha"))

area[[i]] <- ar

}

## convert from list to data.table
area <- rbindlist(area)

area[, c("year", "grid") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

# export df of area
fwrite(area, "output/territory-area.csv")

# Intersection between polygon and points ---------------------------------
intersect_out <- get_intersection(poly = out_polygon, 
                                  spdf = out_spdf,
                                  n = yr$gr_year)

## name lists within intersect_out
names(intersect_out) <- yr$gr_year

## save intersection file
saveRDS(intersect_out, "output/edge_list_data/intersect-pt-poly.RDS")


# Generate edge list based on polygon-point overlap ---------------------------------
edge_out <- get_edgelist(df = intersect_out, 
                         n = yr$gr_year)

edge_list <- rbindlist(edge_out)

# Remove mom-offspring dyads ---------------------------------

## load lifetime data
life <- fread("output/lifetime_clean.csv")

## add column for unique dyad ID of moms and offspring
life$dyad <- as.factor(paste(life$squirrel_id, life$dam_id, sep = "_"))
life$mom <- "yes"

## add column for unique dyad ID
edge_list$dyad <- as.factor(paste(edge_list$owner, edge_list$intruder, sep = "_"))

## merge to form file of just moms and their offspring
mom_offspring <- merge(edge_list, life[,c("dyad", "mom")], by = "dyad")

## generate new edgelist file without moms and their offspring
edge_list <- edge_list %>% 
  filter(!dyad %in% mom_offspring$dyad)

## summary of observations that occurred on own territory vs. on a different territory
edge_list[, c("year", "grid") := tstrsplit(year, "_", fixed=TRUE)]

## export edge list
saveRDS(edge_list, "output/edge_list.RDS")


