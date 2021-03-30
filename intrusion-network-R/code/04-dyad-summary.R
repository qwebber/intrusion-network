

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge_list_data/polygons.RDS")

## KRSP connect
con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                    dbname ="krsp",
                    username = Sys.getenv("krsp_user"),
                    password = Sys.getenv("krsp_password")
)

## load census data
census <- tbl(con, "census") %>% 
  collect() %>%
  filter(gr %in% c("KL", "SU")) %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) 
  
## convert dates
census <- census %>%
              mutate(date=ymd(census_date),
                     julian = yday(census_date),
                     year = year(census_date),
                     squirrel_id = as.factor(squirrel_id))

census$gr_year <- as.factor(paste(census$year, census$gr, sep = "_"))

## subset to only include census dates and remove NAs
census <- setDT(census)[julian == 135 | julian == 136][!is.na(locx)]

## parameters for kernel
params = c(grid = 400, extent = 3)
prj <- '+init=epsg:26911'

#yr <- data.table(gr_year = as.factor(unique(census$gr_year)))

yr <- data.table(gr_year = as.factor(names(polys)))

## generate list of spatial points dataframes
out_spdf <- c()
for(i in levels(yr$gr_year)){ 
  
  df2 <- census[gr_year == i]
  
  spdf <- SpatialPointsDataFrame(coordinates(cbind(df2$locx, df2$locy)),
                                 data = df2[,c("julian","locx", "locy", "squirrel_id")],
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
  edge_list <- data.table(spatial_locs = intersect_out[[i]]$id_polygons,
                          census = intersect_out[[i]]$squirrel_id,
                          locx = intersect_out[[i]]$locx,
                          locy = intersect_out[[i]]$locy,
                          julian = intersect_out[[i]]$julian)
  
  ## assign TRUE or FALSE value to whether a squirrel is observed on 
  ## it's own territory (TRUE) or another territory (FALSE)
  edge_list[, edge:= (spatial_locs==census)]
  
  edge_list$edge <- as.character(edge_list$edge)
  
  ## re-assign TRUE and FALSE values to 0s and 1s
  edge_list$edge[edge_list$edge == "TRUE"] <- 0
  edge_list$edge[edge_list$edge == "FALSE"] <- 1
  
  ## add year to list 
  edge_list$year <- yr$gr_year[i]
  
  ## keep all points in file
  edge_out[[i]] <- edge_list
  
}

edge_list <- rbindlist(edge_out)
edge_list[, .N, by = ""]


## merge file with edges to file with behaviours
df_edges <- merge(life[,c("edge", "territory")], 
                  df[], by = "territory", allow.cartesian = TRUE)

## get rid of duplicate rows
df_edges <- df_edges %>% 
             distinct(.keep_all = TRUE)

## rename behaviours 
df_edges$behaviour[df_edges$behaviour == 0] <- "other"
df_edges$behaviour[df_edges$behaviour == 1] <- "feeding"
df_edges$behaviour[df_edges$behaviour == 2] <- "vocalizations"
df_edges$behaviour[df_edges$behaviour == 3] <- "travelling"
df_edges$behaviour[df_edges$behaviour == 4] <- "resting"
df_edges$behaviour[df_edges$behaviour == 5] <- "in nest"
df_edges$behaviour[df_edges$behaviour == 6] <- "off territory"
df_edges$behaviour[df_edges$behaviour == 7] <- "interaction"
df_edges$behaviour[df_edges$behaviour == 8] <- "caching"
df_edges$behaviour[df_edges$behaviour == 9] <- "dead"
df_edges$behaviour[df_edges$behaviour == 10] <- "grooming"
df_edges$behaviour[df_edges$behaviour == 11] <- "playing"
df_edges$behaviour[df_edges$behaviour == 12] <- "foraging"
df_edges$behaviour[df_edges$behaviour == 13] <- "out of sight"
df_edges$behaviour[df_edges$behaviour == 14] <- "nest building"
df_edges$behaviour[df_edges$behaviour == 15] <- "unknown"
df_edges$behaviour[df_edges$behaviour == 16] <- "scent marking"
df_edges$behaviour[df_edges$behaviour == 17] <- "moving kids"
df_edges$behaviour[df_edges$behaviour == 18] <- "trapped off territory"
df_edges$behaviour[df_edges$behaviour == 19] <- "vigilant"
df_edges$behaviour[df_edges$behaviour == 20] <- "digging for truffles"
df_edges$behaviour[df_edges$behaviour == 21] <- "foot stomping"
df_edges$behaviour[df_edges$behaviour == 22] <- "mating chase"
df_edges$behaviour[df_edges$behaviour == "x"] <- "unknown"
df_edges$behaviour[df_edges$behaviour == ""] <- "unknown"


## summary of behaviours on own territory (0) and other squirrels teritory (1)
sum <- df_edges[, .N, by = c("edge", "behaviour")]
ddply(sum, c('N'))


ggplot()