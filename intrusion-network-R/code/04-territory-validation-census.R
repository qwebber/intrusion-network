

### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge_list_data/polygons.RDS")

## KRSP connect
con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                    dbname ="krsp",
                    username = Sys.getenv("krsp_user"),
                    password = Sys.getenv("krsp_password")
)

#Importing midden census data
census_1 <- tbl(con, "dbamidden") %>% 
  collect() %>%
  dplyr::select(reflo, squirrel_id, locX, locY, grid, date, Sex) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  mutate(locX = loc_to_numeric(locX))

#Importing squirrel census data
census_2 <- tbl(con, "census") %>%
  # be careful with case, database uses locX in census, but locX in dbaMidden
  # sql doesn't care since it's not case sensitive, but R does!
  dplyr::select(reflo, squirrel_id, locx, locy, gr, census_date, sq_fate, sex) %>%
  filter(sq_fate != 7) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locx = loc_to_numeric(locx))

census_2 <- dplyr::select(census_2, -sq_fate) %>% 
  dplyr::rename(locX = locx,
         locY = locy,
         grid = gr,
         date = census_date,
         Sex = sex)

census_all<-bind_rows(census_1, census_2)%>% 
  mutate(grid = factor(grid),
         year = year(ymd(date)),
         month = month(ymd(date)),
         julian = yday(date),
         locY=as.numeric(locY),
         Sex=factor(Sex))

census_all$gr_year <- as.factor(paste(census_all$year, census_all$grid, sep = "_"))

## subset to only include census dates and remove NAs
## exclude years prior to 2000 and KL + SU grids
census_all <- setDT(census_all)[julian == 135 | julian == 136][year >= 2000][grid == "KL" | grid == "SU"]

census_all <- census_all[!is.na(locX)][!is.na(locY)]

######################################################
############ VALIDATION 1: CENSUS MIDDEN #############
######################################################

## parameters for kernel
params = c(grid = 400, extent = 3)
prj <- '+init=epsg:26911'

#yr <- data.table(gr_year = as.factor(unique(census$gr_year)))

yr <- data.table(gr_year = as.factor(names(polys)))


## generate list of spatial points dataframes
out_spdf <- c()
for(i in levels(yr$gr_year)){ 
  
  df2 <- census_all[gr_year == i]
  
  spdf <- SpatialPointsDataFrame(coordinates(cbind(df2$locX, df2$locY)),
                                 data = df2[,c("julian","locX", "locY", "squirrel_id")],
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
                          locx = intersect_out[[i]]$locX,
                          locy = intersect_out[[i]]$locY,
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
edge_list[, c("year", "grid") := tstrsplit(year, "_", fixed=TRUE)]


edge_list[edge == 0][, .N, by = "year"]

## load spatial data
df <- fread("output/spatial-locs.csv")

on_territory <- df[, length(unique(squirrel_id)), by = c("year", "grid")]
setnames(on_territory, "V1", "total_ids")
on_territory$ids_on_terr <- edge_list[edge == 0][, length(unique(census)), by = c("year", "grid")]$V1

## validation of number of squirrel census locs that were on/off territories
on_territory$propOn <- on_territory$ids_on_terr/on_territory$total_ids




## merge file with edges to file with behaviours
df_edges <- merge(life[,c("edge", "territory")], 
                  df[], by = "territory", allow.cartesian = TRUE)

## get rid of duplicate rows
df_edges <- df_edges %>% 
             distinct(.keep_all = TRUE)

## rename behaviours 
df$behaviour[df$behaviour == 0] <- "other"
df$behaviour[df$behaviour == 1] <- "feeding"
df$behaviour[df$behaviour == 2] <- "vocalizations"
df$behaviour[df$behaviour == 3] <- "travelling"
df$behaviour[df$behaviour == 4] <- "resting"
df$behaviour[df$behaviour == 5] <- "in nest"
df$behaviour[df$behaviour == 6] <- "off territory"
df$behaviour[df$behaviour == 7] <- "interaction"
df$behaviour[df$behaviour == 8] <- "caching"
df$behaviour[df$behaviour == 9] <- "dead"
df$behaviour[df$behaviour == 10] <- "grooming"
df$behaviour[df$behaviour == 11] <- "playing"
df$behaviour[df$behaviour == 12] <- "foraging"
df$behaviour[df$behaviour == 13] <- "out of sight"
df$behaviour[df$behaviour == 14] <- "nest building"
df$behaviour[df$behaviour == 15] <- "unknown"
df$behaviour[df$behaviour == 16] <- "scent marking"
df$behaviour[df$behaviour == 17] <- "moving kids"
df$behaviour[df$behaviour == 18] <- "trapped off territory"
df$behaviour[df$behaviour == 19] <- "vigilant"
df$behaviour[df$behaviour == 20] <- "digging for truffles"
df$behaviour[df$behaviour == 21] <- "foot stomping"
df$behaviour[df$behaviour == 22] <- "mating chase"
df$behaviour[df$behaviour == "x"] <- "unknown"
df$behaviour[df$behaviour == ""] <- "unknown"


## summary of behaviours on own territory (0) and other squirrels teritory (1)
sum <- df[, .N, by = c("behaviour")]
plyr::ddply(sum, c('N'))


ggplot(census[gr_year == "2012_KL"]) + 
  geom_jitter(aes(locx, locy, color = squirrel_id), height = 0.2, width = 0.3) 
  theme(legend.position = 'none')
