

######################################################
####### VALIDATION 2: POST-RELEASE RATTLES ###########
######################################################

### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)


## load polygon data
polys <- readRDS("output/edge_list_data/polygons.RDS")

## load spatial data
df <- fread("output/spatial-locs.csv")

## subset to trapping data and exclude NAs
trp <- df[data == "trap"]
trp$rattle[trp$rattle == ""] <- NA
trp <- trp[!is.na(rattle)]

## assign gr_year variable
trp$gr_year <- as.factor(paste(trp$year, trp$gr, sep = "_"))


## generate list of spatial points dataframes
yr <- data.table(gr_year = as.factor(names(polys)))
prj <- '+init=epsg:26911'

out_spdf <- c()
for(i in levels(yr$gr_year)){ 
  
  df2 <- trp[gr_year == i]
  
  spdf <- SpatialPointsDataFrame(coordinates(cbind(df2$locx, df2$locy)),
                                 data = df2[,c("julian","locx", "locy", "squirrel_id", "rattle")],
                                 proj4string = CRS(prj))
  
  out_spdf[[i]] <- st_as_sf(spdf)
  
}

# Intersection between polygon and points ---------------------------------
intersect_out <- c()
for(i in 1:length(yr$gr_year)){
  
  intersection <- st_intersection(x = polys[[i]], y = out_spdf[[i]])
  
  intersect_out[[i]] <- intersection 
}

## name lists within intersect_out
names(intersect_out) <- yr$gr_year


edge_out <- c()
for(i in 1:length(yr$gr_year)){ 
  ## generate edge list with territory owners and intruders
  edge_list <- data.table(territory_owner = intersect_out[[i]]$id_polygons,
                          trapped_squirrel = intersect_out[[i]]$squirrel_id,
                          locx = intersect_out[[i]]$locx,
                          locy = intersect_out[[i]]$locy,
                          julian = intersect_out[[i]]$julian,
                          rattle = intersect_out[[i]]$rattle)
  
  ## assign TRUE or FALSE value to whether a squirrel is observed on 
  ## it's own territory (TRUE) or another territory (FALSE)
  edge_list[, edge:= (territory_owner==trapped_squirrel)]
  
  edge_list$edge <- as.character(edge_list$edge)
  
  ## re-assign TRUE and FALSE values to 0s and 1s
  edge_list$edge[edge_list$edge == "TRUE"] <- "own_territory"
  edge_list$edge[edge_list$edge == "FALSE"] <- "other_territory"
  
  ## add year to list 
  edge_list$year <- yr$gr_year[i]
  
  ## keep all points in file
  edge_out[[i]] <- edge_list
  
}

edge_list <- rbindlist(edge_out)
edge_list[, c("year", "grid") := tstrsplit(year, "_", fixed=TRUE)]


ggplot(edge_list) +
  geom_jitter(aes(rattle, edge, color = rattle)) +
  facet_wrap(~grid)


rattle_sum <- edge_list[, .N, by = c("rattle", "edge")]


M_rattle <- as.table(rbind(c(rattle_sum[rattle == "R"][edge == "other_territory"]$N),
                           c(rattle_sum[rattle == "R"][edge == "own_territory"]$N)))

dimnames(M_rattle) <- list(territory = c("other", "own"),
                    behaviour = c("rattle"))
  
(Xsq <- chisq.test(M_rattle))

M_stay <- as.table(rbind(c(rattle_sum[rattle == "HA"][edge == "other_territory"]$N),
                           c(rattle_sum[rattle == "HA"][edge == "own_territory"]$N)))

dimnames(M_stay) <- list(territory = c("other", "own"),
                           behaviour = c("stay"))

(Xsq <- chisq.test(M_stay))

  