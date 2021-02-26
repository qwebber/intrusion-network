

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- fread("output/spatial-locs-2016.csv")

## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

## drop all squirrels with <30 observations
df <- df[N > 30]

## order dataframe by grid and year
df <- setDT(ddply(df, c('year', 'grid')))

## check to make sure there are no outliers
ggplot(df) +
  geom_point(aes(locx, locy, color = factor(squirrel_id))) +
  theme(legend.position = 'none') +
  facet_wrap(~year*grid)

df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.factor(paste(df$year, df$grid, sep = "_"))

## prj
prj <- '+init=epsg:26911'

## load GetHRBy function
source("functions/GetHRBy.R")

## parameters for kernel
params = c(grid = 400, extent = 3)

yr <- data.table(gr_year = as.factor(unique(df$gr_year)))

fwrite(yr, "output/unique-grid-years.csv")

## generate list of spatial points dataframes
out_spdf <- c()
for(i in levels(yr$gr_year)){ 
  
  df2 <- df[gr_year == i]
  
  spdf <- SpatialPointsDataFrame(coordinates(cbind(df2$locx, df2$locy)),
                              data = df2[,c("locx", "locy", "squirrel_id")],
                                        proj4string = CRS(prj))
  
  out_spdf[[i]] <- st_as_sf(spdf)
  
}

saveRDS(out_spdf, "output/edge_list_data/spdf.RDS")

## generate list of kernels
out_polygon <- c()
for(i in levels(yr$gr_year)){ 
  
  df3 <- df[gr_year == i]
  
  ## generate ranges by ID
  ud <- df3[, GetHRBy(squirrel_id, locx, locy, 
                          in.percent = 75, params = params,
                          type = "kernel")]

  ## assign prj
  proj4string(ud) <- CRS(prj)

  # Get polygon
  polygon <- st_as_sf(ud)

  # convert to sf object
  colnames(polygon) <- c("id_polygons", "area" ,"geometry") # change colnames

  out_polygon[[i]] <- polygon

}

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
intersect_out <- c()
for(i in 1:length(yr$gr_year)){
  
  intersection <- st_intersection(x = out_polygon[[i]], y = out_spdf[[i]])
  
  intersect_out[[i]] <- intersection 
}

## name lists within intersect_out
names(intersect_out) <- yr$gr_year

saveRDS(intersect_out, "output/edge_list_data/intersect-pt-poly.RDS")

edge_out <- c()
for(i in 1:length(yr$gr_year)){ 
  ## generate edge list with territory owners and intruders
  edge_list <- data.table(owner = intersect_out[[i]]$id_polygons,
                          intruder = intersect_out[[i]]$squirrel_id)

  ## assign TRUE or FALSE value to whether a squirrel is observed on 
  ## it's own territory (TRUE) or another territory (FALSE)
  edge_list[, edge:= (owner==intruder)]

  edge_list$edge <- as.character(edge_list$edge)

  ## re-assign TRUE and FALSE values to 0s and 1s
  edge_list$edge[edge_list$edge == "TRUE"] <- 0
  edge_list$edge[edge_list$edge == "FALSE"] <- 1

  ## add year to list 
  edge_list$year <- yr$gr_year[i]
  
  ## subset to only include intrusion events 
  edge_out[[i]] <- edge_list[edge == 1]

}

## export edge list
saveRDS(edge_out, "output/edge_list.RDS")


