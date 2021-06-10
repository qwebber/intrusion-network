

### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp', 'spatsoc')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

## load census data
census_all <- readRDS("output/auxilliary-data/census-all.RDS")
census_all <- census_all[!is.na(census_all$squirrel_id)]
setnames(census_all, c("locX", "locY"), c("locx", "locy"))

census_all$locx <- census_all$locx*30
census_all$locy <- census_all$locy*30

census_all$squirrel_id <- as.character(census_all$squirrel_id)

## load home made functions
source("functions/get_spdf.R")
source("functions/get_buffer.R")
source("functions/get_intersection.R")
source("functions/get_edgelist.R")


######################################################
############ VALIDATION 1: CENSUS MIDDEN #############
######################################################

## parameters for kernel
params = c(grid = 400, extent = 7)
prj <- '+init=epsg:26911'

yr <- fread("output/unique-grid-years.csv")

n = length(unique(yr$gr_year))

# Generate spdf points ---------------------------------
out_spdf <- get_spdf(df = census_all[,c("locx", "locy", "squirrel_id", "gr_year")], 
                     n = n,
                     yr = yr)

# Generate buffer around spdf points ---------------------------------
buf_spdf <- get_buffer(df = out_spdf,
                       n = n,
                       buff = 15)

# Intersection between polygon and points ---------------------------------
intersect_out <- get_intersection(poly = polys, 
                                  spdf = buf_spdf,
                                  n = n)

# Generate edge list based on polygon-point overlap ---------------------------------
edge_out <- get_edgelist(df = intersect_out, 
                         n = yr$gr_year)

edge_list <- rbindlist(edge_out)
setnames(edge_list, "year", "gr_year")

setnames(edge_list, c("owner", "intruder"), c("polygon", "census"))

edge_list[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]
edge_list <- edge_list[gr_year != "KL_2006"]

edge_list[, .N, by = c("gr_year", "edge")]

## edge == 0 are territories generating using polygon and from the census that are paired
edge_list_own <- edge_list[edge == 0]

## load spatial data
df <- readRDS("output/spatial-locs.RDS")
df <- df[gr_year != "KL_2006"]
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)
df$id_yr_gr <- as.factor(paste(df$squirrel_id, df$gr_year, sep = "_"))

## load final data file and subset from spatial locs 
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

## remove individuals that don't make it to final DF
df <- merge(df, all[,c("id_yr_gr")], by = "id_yr_gr")

on_territory <- df[, length(unique(squirrel_id)), by = c("gr_year")]
setnames(on_territory, "V1", "total_ids")

edge_list_own$id_yr_gr <- as.factor(paste(edge_list_own$census, edge_list_own$gr_year, sep = "_"))
edge_list_own <- merge(edge_list_own, all[,c("id_yr_gr")], by = "id_yr_gr")
edge_list_own2 <- edge_list_own[, length(unique(census)), by = c("gr_year")]

on_territory <- merge(on_territory, edge_list_own2, by = "gr_year")
setnames(on_territory, "V1", "ids_on_terr")

## validation of number of squirrel census locs that were on/off territories
on_territory$propOn <- on_territory$ids_on_terr/on_territory$total_ids
on_territory$off <- on_territory$total_ids - on_territory$ids_on_terr
on_territory[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]

on_territory[, sum(total_ids), by = "grid"]
on_territory[, sum(ids_on_terr ), by = "grid"]
## KL: 
1011/1132
## SU: 
620/673


### CALCULATE DISTANCE BETWEEN CENSUS MIDDEN AND KDE CENTROID 

## calculate mean for X and Y locs from spatial locs file
locXm <- df[, mean(locx), by = c("squirrel_id","gr_year")]
setnames(locXm, "V1", "locx")
locXm$id_yr_gr <- as.factor(paste(locXm$squirrel_id, locXm$gr_year, sep = "_"))
locYm <- df[, mean(locy), by = c("squirrel_id","gr_year")]
setnames(locYm, "V1", "locy")
locYm$id_yr_gr <- as.factor(paste(locYm$squirrel_id, locYm$gr_year, sep = "_"))

locM <- merge(locXm, locYm[,c("gr_year", "squirrel_id") := NULL], by = "id_yr_gr")
locM$data <- "spatial"

## calculate mean for X and Y locs from midden locs in census
censusXm <- census_all[, mean(locx), by = c("squirrel_id","gr_year")]
setnames(censusXm, "V1", "locx")
censusXm$id_yr_gr <- as.factor(paste(censusXm$squirrel_id, censusXm$gr_year, sep = "_"))
censusYm <- census_all[, mean(locy), by = c("squirrel_id","gr_year")]
setnames(censusYm, "V1", "locy")
censusYm$id_yr_gr <- as.factor(paste(censusYm$squirrel_id, censusYm$gr_year, sep = "_"))

censusM <- merge(censusXm, censusYm[,c("gr_year", "squirrel_id") := NULL], by = "id_yr_gr")
censusM$data <- "census"

## only include midden locs who met criteria for territories
censusM2 <- merge(censusM, locM[, c("id_yr_gr")], by = "id_yr_gr")

pairLocsAll <- rbind(locM, censusM2)

## calculate distance between locs
edgeDist <- edge_dist(pairLocsAll, id = "squirrel_id", coords = c("locx", "locy"), 
                      timegroup = NULL, threshold = 10000, returnDist = T, 
                      splitBy = "gr_year")


edgeDist[, self := (ID1 == ID2)]
edgeDist$self <- as.character(edgeDist$self)
edgeDist$self[edgeDist$self == "FALSE"] <- 1
edgeDist$self[edgeDist$self == "TRUE"] <- 0

edgeDist2 <- edgeDist[self == 0]
edgeDist2 <- edgeDist2[!duplicated(edgeDist2)]
edgeDist2$id_yr_gr <- as.factor(paste(edgeDist2$ID1, edgeDist2$gr_year, sep = "_"))

edgeDist2[, max(distance), by = "gr_year"]

edge_list$id_yr_gr <- as.factor(paste(edge_list$census, edge_list$gr_year, sep = "_"))

#### Distances for squirrels whose midden was "on" the territorial KDE 
distOn <- merge(edgeDist2, edge_list[edge == 0][,c("id_yr_gr")], by = "id_yr_gr")
distOn <- distOn[!duplicated(distOn)]
distOn[, .N, by = "gr_year"]


edge_list_own_id <- edge_list[, length(unique(census)), by = c("census", "gr_year", "edge")]
edge_list_own_id$id_yr_gr <- as.factor(paste(edge_list_own_id$census, edge_list_own_id$gr_year, sep = "_"))

distAll <- merge(edgeDist2, edge_list_own_id[,c("gr_year") := NULL], by = "id_yr_gr", all = T)
distAll$V1[is.na(distAll$V1)] <- 0

## check number of squirrels with middens off KDE per gr/year
distAll[V1 == 0][, uniqueN(ID1), by = c("gr_year")]

#### 
ggplot() +#subset(polys$KL_1999)) + 
             # id_polygons == 5763 |
              #id_polygons == 5755 |
              #id_polygons == 5587 |
              #id_polygons == 5518 | 
              #id_polygons == 4818 )) +
  #geom_sf(aes(color = id_polygons), alpha = 0.5) +
  geom_point(data = pairLocsAll[gr_year == "KL_2007"], #[squirrel_id == 5763 |
               #squirrel_id == 5755 |
                # squirrel_id == 5587 |
                 #squirrel_id == 5518 | 
                 #squirrel_id == 4818 ],
             aes(locx, locy, 
                 color = squirrel_id, 
                 shape = data), ## shape = census data or centroid from spatial data 
             # if circle is outside of poylgon that means the midden is not considered "on" the KDE territory
             alpha = 0.5) +
  scale_color_viridis_d() +
  theme(panel.grid.minor = element_line(color = "grey"),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 0.5))



## Run two years by hand - something broken in the get_intersection function

### KL 1999
KL1999 <- st_intersection(x = polys$KL_1999, y = out_spdf[[6]])
KL1999dt <- data.table(owner = KL1999$id_polygons, 
                       intruder = KL1999$squirrel_id, ## id must be squirrel_id
                       locx = KL1999$locx, ## must be locx
                       locy = KL1999$locy, ## must be locy
                       julian = KL1999$julian) ## ## must be julian

## assign TRUE or FALSE value to whether a squirrel is observed on 
## it's own territory (TRUE) or another territory (FALSE)
KL1999dt[, edge:= (owner==intruder)]

KL1999dt$edge <- as.character(KL1999dt$edge)

## re-assign TRUE and FALSE values to 0s and 1s
KL1999dt$edge[KL1999dt$edge == "TRUE"] <- 0
KL1999dt$edge[KL1999dt$edge == "FALSE"] <- 1
length(unique(KL1999dt[edge == 0]$owner))

### KL 2004
KL2004 <- st_intersection(x = polys$KL_2004, y = out_spdf[[16]])
KL2004dt <- data.table(owner = KL2004$id_polygons, 
                       intruder = KL2004$squirrel_id, ## id must be squirrel_id
                       locx = KL2004$locx, ## must be locx
                       locy = KL2004$locy, ## must be locy
                       julian = KL2004$julian) ## ## must be julian

## assign TRUE or FALSE value to whether a squirrel is observed on 
## it's own territory (TRUE) or another territory (FALSE)
KL2004dt[, edge:= (owner==intruder)]

KL2004dt$edge <- as.character(KL2004dt$edge)

## re-assign TRUE and FALSE values to 0s and 1s
KL2004dt$edge[KL2004dt$edge == "TRUE"] <- 0
KL2004dt$edge[KL2004dt$edge == "FALSE"] <- 1
length(unique(KL2004dt[edge == 0]$owner))

length(unique(polys$KL_2004$id_polygons))


### SU 1996
SU1996 <- st_intersection(x = polys$SU_1996, y = out_spdf[[2]])
SU1996dt <- data.table(owner = SU1996$id_polygons, 
                       intruder = SU1996$squirrel_id, ## id must be squirrel_id
                       locx = SU1996$locx, ## must be locx
                       locy = SU1996$locy, ## must be locy
                       julian = SU1996$julian) ## ## must be julian

## assign TRUE or FALSE value to whether a squirrel is observed on 
## it's own territory (TRUE) or another territory (FALSE)
SU1996dt[, edge:= (owner==intruder)]

SU1996dt$edge <- as.character(SU1996dt$edge)

## re-assign TRUE and FALSE values to 0s and 1s
SU1996dt$edge[SU1996dt$edge == "TRUE"] <- 0
SU1996dt$edge[SU1996dt$edge == "FALSE"] <- 1
length(unique(SU1996dt[edge == 0]$owner))