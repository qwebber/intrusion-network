

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

df <- df[order(df$year), ]

## prj
prj <- '+init=epsg:26911'

## load home-made functions
source("functions/get_spdf.R")
source("functions/get_polygon.R")

## generate list of grid-year combinations
yr <- data.table(gr_year = as.character(unique(df$gr_year)))
fwrite(yr, "output/unique-grid-years.csv")

n = length(unique(yr$gr_year))

# Generate spdf points ---------------------------------
out_spdf <- get_spdf(df = df[,c("julian","locx", "locy", "squirrel_id", "gr_year")], 
               n = n,
               yr = yr)

names(out_spdf) <- yr$gr_year

## save SPDF
saveRDS(out_spdf, "output/edge-list-inputs/spdf.RDS")

## assign squirrel_id_yr column
df$squirrel_id_yr <- as.factor(paste(df$squirrel_id, df$gr_year, sep = "_"))

## parameters for kernel
params = c(grid = 400, extent = 7)

## subset to 1996 - 1999
df_poly90s <- df[year < 2000]
df_poly90s <- df_poly90s[order(df_poly90s$year), ]

yr90s <- data.table(gr_year = as.character(unique(df_poly90s$gr_year)))
n90s = length(unique(yr90s$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon90s <- get_polygon(input = df_poly90s, 
                              n = n90s,
                              yr = yr90s,
                              in.percent = 50,
                              params = params)

names(out_polygon90s) <- yr90s$gr_year

saveRDS(out_polygon90s, "output/edge-list-inputs/polygons-1996-99.RDS")

## 2000 - 2005
df_poly2005 <- df[year >= 2000 & year <= 2005]

yr05 <- data.table(gr_year = as.character(unique(df_poly2005$gr_year)))
n05 = length(unique(yr05$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon05 <- get_polygon(input = df_poly2005, 
                           n = n05,
                           yr = yr05,
                            in.percent = 50,
                            params = params)

names(out_polygon05) <- yr05$gr_year

saveRDS(out_polygon05, "output/edge-list-inputs/polygons-2000-2005.RDS")

## 2006 - 2010
df_poly2010 <- df[year >= 2006 & year <= 2010]

yr10 <- data.table(gr_year = as.character(unique(df_poly2010$gr_year)))
n10 = length(unique(yr10$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon10 <- get_polygon(input = df_poly2010, 
                             n = n10,
                             yr = yr10,
                             in.percent = 50,
                             params = params)

names(out_polygon10) <- yr10$gr_year

saveRDS(out_polygon10, "output/edge-list-inputs/polygons-2006-2010.RDS")

## 2011 - 2015
df_poly2015 <- df[year >= 2011 & year <= 2015]

yr15 <- data.table(gr_year = as.character(unique(df_poly2015$gr_year)))
n15 = length(unique(yr15$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon15 <- get_polygon(input = df_poly2015, 
                             n = n15,
                             yr = yr15,
                             in.percent = 50,
                             params = params)

names(out_polygon15) <- yr15$gr_year

saveRDS(out_polygon15, "output/edge-list-inputs/polygons-2011-2015.RDS")

## 2016 - 2020

## remove problem individuals
df_poly2020 <- df[year >= 2016 & 
                    squirrel_id_yr != "12027_KL_2016" &
                    squirrel_id_yr != "12028_KL_2016"]

## new file with just problem individuals
df_probs <- df[squirrel_id_yr == "12027_KL_2016" |
                  squirrel_id_yr == "12028_KL_2016"]

df_probs$locx <- df_probs$locx + sample(1:10, size = length(df_probs$locx), replace = T)
df_probs$locy <- df_probs$locy + sample(1:10, size = length(df_probs$locy), replace = T)

df_poly2020 <- rbind(df_probs,df_poly2020)

yr20 <- data.table(gr_year = as.character(unique(df_poly2020$gr_year)))
n20 = length(unique(yr20$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon20 <- get_polygon(input = df_poly2020, 
                             n = n20,
                             yr = yr20,
                             in.percent = 50,
                             params = params)

names(out_polygon20) <- yr20$gr_year

saveRDS(out_polygon20, "output/edge-list-inputs/polygons-2016-2020.RDS")

