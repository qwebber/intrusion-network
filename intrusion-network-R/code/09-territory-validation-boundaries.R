


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

#Convert locX, locY into spatial points and associate with a SpatialPointsDataFrame.
##KL pass
kllocmat <- matrix(c(df$locx, df$locy), ncol=2) # get spatial locs into a matrix

kllocsp <- SpatialPoints(kllocmat) # from {sp} Make the matrix spatial 

str(kllocsp) #look at structure
kllocsp.df <- data.frame(kllocsp) #make into data frame

plot(kllocsp) #Visualize the points. Should mirror the plots from earlier
summary(kllocsp) # Get a summary of how R sees these points
coordinates(kllocsp)

## R *really* wants you to have defined a CRS (Coordinate Reference System)

is.projected(kllocsp) #see if a projection is defined 
# Returns `NA` if no geographic coordinate system or projection; returns
# FALSE if has geographic coordinate system but no projection.

crs.geo <- CRS("+init=EPSG:3578")  #we use NAD83 / Yukon Albers here
proj4string(kllocsp) <- crs.geo  # define projection system of our data
is.projected(kllocsp) #Check projection
summary(kllocsp)

kllocsp2 <- spTransform(kllocsp,CRS("+init=EPSG:3578"))

plot(kllocsp)

#Add Attributes
kl.spdf <- SpatialPointsDataFrame(kllocsp2, df)
summary(kl.spdf)

kl.spdf.KL17 <- subset(kl.spdf, gr_year == "KL_2017" | gr_year == "SU_2017")
yr1 <- data.table(gr_year = as.character(unique(kl.spdf.KL17$gr_year)))
n1 <- length(unique(kl.spdf.KL17$gr_year))


# Generate territorial polygons ---------------------------------
out_polygon_locs <- get_polygon(input = kl.spdf.KL17, 
                           n = n1,
                           yr = yr1,
                           in.percent = 50,
                           params = params)

## select individuals from polys where territories were generated in the field

core_terr <- fread("output/territories/input/squordinates2017core.csv")
core_terr[, c("squirrel_id", "year") := tstrsplit(sqyear, "_", fixed=TRUE)][,c("sqyear") := NULL]
core_terr$gr_year <- as.factor(paste(core_terr$gr, core_terr$year, sep = ""))


## load home-made functions
source("functions/get_polygon.R")

## parameters for kernel
params = c(grid = 400, extent = 7)

## prj
prj <- '+init=epsg:26911'

yr <- data.table(gr_year = as.character(unique(core_terr$gr_year)))
n = length(unique(core_terr$gr_year))

setnames(core_terr, c("lon", "lat"), c("locx", "locy"))

# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(input = core_terr, 
                              n = n,
                              yr = yr,
                              in.percent = 50,
                              params = params)

names(out_polygon) <- yr$gr_year

ggplot() +
  geom_sf(data = out_polygon$KL2017, aes(fill = id_polygons)) +
  geom_sf(data = polys$KL_2017, aes(fill = id_polygons), alpha = 0.25)  +
  theme(legend.position = 'none')

ggplot(kl.spdf) +
  geom_point(aes(locx, locy, color = squirrel_id))
