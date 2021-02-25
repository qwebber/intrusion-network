

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- fread("output/spatial-locs-2016.csv")

## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df$row <- 1:nrow(df)
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

## drop all squirrels with <30 observations
df <- df[N > 30]

## check to make sure there are no outliers
ggplot(df) +
  geom_point(aes(locx, locy, color = factor(squirrel_id)))

df$squirrel_id <- as.factor(df$squirrel_id)

spdf <- SpatialPointsDataFrame(coordinates(cbind(df$locx, df$locy)),
                              data = df[,c("locx", "locy", "squirrel_id")])

source("functions/GetHRBy.R")

ud <- setDT(df)[, GetHRBy(squirrel_id, locx, locy, 50 ,"kernel")]

# Get polygon
polygon <- st_as_sf(ud)
# convert to sf object
colnames(polygon) <- c("id_polygons", "area" ,"geometry") # change colnames
#polygon$id_polygons <- paste0("poly_", polygon$id_polygons) #  change polygon ID

## output area
area <- data.table(polygon$id_polygons, polygon$area)

## drop area 
drops <- c("area") # list of column names
polygon <- polygon[,!(names(polygon) %in% drops)] #remove columns "name1" and "name2"

## convert points to sf
points <- st_as_sf(spdf)

# Intersection between polygon and points ---------------------------------

intersection <- st_intersection(x = polygon, y = points)

edge_list <- data.table(owner = intersection$id_polygons,
                        intruder = intersection$squirrel_id)

## assign TRUE or FALSE value to whether a squirrel is observed on 
## it's own territory (TRUE) or another territory (FALSE)
edge_list[, edge:= (owner==intruder)]

edge_list$edge <- as.character(edge_list$edge)

## re-assign TRUE and FALSE values to 0s and 1s
edge_list$edge[edge_list$edge == "TRUE"] <- 0
edge_list$edge[edge_list$edge == "FALSE"] <- 1

## subset to only include intrusion events 
edge_list <- edge_list[edge == 1][,c("edge") := NULL]


grph <- graph_from_edgelist(as.matrix(edge_list), directed=T)


graph.strength(grph, mode = c("out"))
