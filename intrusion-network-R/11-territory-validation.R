

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)


df <- fread("output/spatial-locs-15.csv")
df$squirrel_id <- as.character(df$squirrel_id)

yr <- fread("output/unique-grid-years.csv")

## prj
prj <- '+init=epsg:26911'

## load home made packages
source("functions/get_spdf.R")
source("functions/get_polygon.R")

## load SPDF file
## save SPDF
spdf <- readRDS("output/edge_list_data/spdf.RDS")

## subset to KL 2015 and 2016
spdf2 <- list(spdf[[21]], spdf[[22]])

yr2 <- data.table(gr_year = c("KL_2015", "KL_2016"))
## re-assign names 
names(polys) <- yr2$gr_year

## parameters for kernel
params = c(grid = 400, extent = 7)

# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(df = polys, 
                           n = yr2$gr_year,
                           in.percent = 50,
                           params = params)
