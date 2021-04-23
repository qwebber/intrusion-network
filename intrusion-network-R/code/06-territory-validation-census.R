

### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

## load census data
census_all <- readRDS("output/auxilliary-data/census-all.RDS")
census_all <- census_all[!is.na(census_all$squirrel_id)]
setnames(census_all, c("locX", "locY"), c("locx", "locy"))

census_all$locx <- census_all$locx*30
census_all$locy <- census_all$locy*30

## load home made functions
source("functions/get_spdf.R")
source("functions/get_intersection.R")
source("functions/get_edgelist.R")

######################################################
############ VALIDATION 1: CENSUS MIDDEN #############
######################################################

## parameters for kernel
params = c(grid = 400, extent = 3)
prj <- '+init=epsg:26911'

yr <- fread("output/unique-grid-years.csv")

n = length(unique(yr$gr_year))

# Generate spdf points ---------------------------------
out_spdf <- get_spdf(df = census_all[,c("julian","locx", "locy", "squirrel_id", "gr_year")], 
                     n = n,
                     yr = yr)

# Intersection between polygon and points ---------------------------------
intersect_out <- get_intersection(poly = polys, 
                                  spdf = out_spdf,
                                  n = n)

# Generate edge list based on polygon-point overlap ---------------------------------
edge_out <- get_edgelist(df = intersect_out, 
                         n = yr$gr_year)

edge_list <- rbindlist(edge_out)

setnames(edge_list, c("owner", "intruder"), c("polygon", "census"))

edge_list[, c("grid", "year") := tstrsplit(year, "_", fixed=TRUE)]


edge_list[edge == 0][, .N, by = "year"]

## load spatial data
df <- readRDS("output/spatial-locs.RDS")
df <- df[gr_year != "KL_1996" & 
           gr_year != "SU_1998" & 
           gr_year != "KL_1999" & 
           gr_year != "KL_2016"]


on_territory <- df[, length(unique(squirrel_id)), by = c("year", "grid")]
setnames(on_territory, "V1", "total_ids")
on_territory$ids_on_terr <- edge_list[edge == 0][, length(unique(census)), by = c("year", "grid")]$V1

## validation of number of squirrel census locs that were on/off territories
on_territory$propOn <- on_territory$ids_on_terr/on_territory$total_ids

on_territory[grid == "KL"]
on_territory[grid == "SU"]

mean(on_territory$propOn)
sd(on_territory$propOn)
