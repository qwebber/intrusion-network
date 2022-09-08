


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp', 'spatsoc',
          'lubridate','ggplot2', 'krsp', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)

## load data
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

## prj
prj <- '+init=epsg:26911'

## calculate territory overlap for 30% kernels
terr_over30 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 30),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over30$proportion <- round(terr_over30$proportion/10000, 3)
terr_over30$area <- round(terr_over30$area/10000, 4)
terr_over30[,same := (squirrel_id == squirrel_id2)]
terr_over30 <- terr_over30[same != TRUE]
terr_over30$percent <- 30

## calculate territory overlap for 40% kernels
terr_over40 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 40),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over40$proportion <- round(terr_over40$proportion/10000, 3)
terr_over40$area <- round(terr_over40$area/10000, 4)
terr_over40[,same := (squirrel_id == squirrel_id2)]
terr_over40 <- terr_over40[same != TRUE]
terr_over40$percent <- 40

## calculate territory overlap for 50% kernels
terr_over50 <- group_polys(df[gr_year == "KL_2002" | 
                             gr_year == "KL_2004" |
                             gr_year == "KL_2006" |
                             gr_year == "KL_2009" |
                             gr_year == "KL_2014" |
                             gr_year == "KL_2020"], 
                        area = TRUE,
                        hrType = 'kernel',
                        hrParams = list(percent = 50),
                        id = 'squirrel_id',
                        coords = c("locx", "locy"),
                        projection = prj, 
                        splitBy = c("grid", "year"))

terr_over50$proportion <- round(terr_over50$proportion/10000, 3)
terr_over50$area <- round(terr_over50$area/10000, 4)
terr_over50[,same := (squirrel_id == squirrel_id2)]
terr_over50 <- terr_over50[same != TRUE]
terr_over50$percent <- 50

## calculate territory overlap for 60% kernels
terr_over60 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 60),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over60$proportion <- round(terr_over60$proportion/10000, 3)
terr_over60$area <- round(terr_over60$area/10000, 4)
terr_over60[,same := (squirrel_id == squirrel_id2)]
terr_over60 <- terr_over60[same != TRUE]
terr_over60$percent <- 60

## calculate territory overlap for 70% kernels
terr_over70 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 70),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over70$proportion <- round(terr_over70$proportion/10000, 3)
terr_over70$area <- round(terr_over70$area/10000, 4)
terr_over70[,same := (squirrel_id == squirrel_id2)]
terr_over70 <- terr_over70[same != TRUE]
terr_over70$percent <- 70

## calculate territory overlap for 80% kernels
terr_over80 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 80),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over80$proportion <- round(terr_over80$proportion/10000, 3)
terr_over80$area <- round(terr_over80$area/10000, 4)
terr_over80[,same := (squirrel_id == squirrel_id2)]
terr_over80 <- terr_over80[same != TRUE]
terr_over80$percent <- 80

## calculate territory overlap for 95% kernels
terr_over90 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"], 
                           area = TRUE,
                           hrType = 'kernel',
                           hrParams = list(percent = 90),
                           id = 'squirrel_id',
                           coords = c("locx", "locy"),
                           projection = prj, 
                           splitBy = c("grid", "year"))

terr_over90$proportion <- round(terr_over90$proportion/10000, 3)
terr_over90$area <- round(terr_over90$area/10000, 4)
terr_over90[,same := (squirrel_id == squirrel_id2)]
terr_over90 <- terr_over90[same != TRUE]
terr_over90$percent <- 90


## bind together files
terr_overlap_all <- rbind(terr_over30, terr_over40, 
                          terr_over50, terr_over60, 
                          terr_over70, terr_over80, 
                          terr_over90)
terr_overlap_all$year <- as.factor(terr_overlap_all$year)

saveRDS(terr_overlap_all, "output/territories/territory-overlap.RDS")

### Generate territory size
source("functions/get_polygon.R")

## parameters for kernel
params = c(grid = 400, extent = 7)

## only run polygons for six years
df_polys <- df[gr_year == "KL_2002" | 
                   gr_year == "KL_2004" |
                   gr_year == "KL_2006" |
                   gr_year == "KL_2009" |
                   gr_year == "KL_2014" |
                   gr_year == "KL_2020"]

yr <- data.table(gr_year = as.character(unique(df_polys$gr_year)))
n = length(unique(yr$gr_year))


# Generate 30% territorial polygons ---------------------------------
out_polygon30 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 30,
                             params = params)


area30 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon30[[i]]$id_polygons, out_polygon30[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area30[[i]] <- ar
  
}

## convert from list to data.table
area30 <- rbindlist(area30)
area30$percent <- 30


# Generate 40% territorial polygons ---------------------------------
out_polygon40 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 40,
                             params = params)


area40 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon40[[i]]$id_polygons, out_polygon40[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area40[[i]] <- ar
  
}

## convert from list to data.table
area40 <- rbindlist(area40)
area40$percent <- 40


# Generate 50% territorial polygons ---------------------------------
out_polygon50 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 50,
                             params = params)


area50 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon50[[i]]$id_polygons, out_polygon50[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area50[[i]] <- ar
  
}

## convert from list to data.table
area50 <- rbindlist(area50)
area50$percent <- 50


# Generate 60% territorial polygons ---------------------------------
out_polygon60 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 60,
                             params = params)


area60 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon60[[i]]$id_polygons, out_polygon60[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area60[[i]] <- ar
  
}

## convert from list to data.table
area60 <- rbindlist(area60)
area60$percent <- 60

# Generate 70% territorial polygons ---------------------------------
out_polygon70 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 70,
                             params = params)


area70 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon70[[i]]$id_polygons, out_polygon70[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area70[[i]] <- ar
  
}

## convert from list to data.table
area70 <- rbindlist(area70)
area70$percent <- 70

# Generate 80% territorial polygons ---------------------------------
out_polygon80 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 80,
                             params = params)


area80 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon80[[i]]$id_polygons, out_polygon80[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area80[[i]] <- ar
  
}

## convert from list to data.table
area80 <- rbindlist(area80)
area80$percent <- 80

# Generate 90% territorial polygons ---------------------------------
out_polygon90 <- get_polygon(input = df_polys, 
                             n = n,
                             yr = yr,
                             in.percent = 90,
                             params = params)


area90 <- c()
for(i in 1:length(yr$gr_year)){ 
  ar <- data.table(out_polygon90[[i]]$id_polygons, out_polygon90[[i]]$area)
  ar$gr_year <- yr$gr_year[i]
  setnames(ar, c("V1", "V2"), c("squirrel_id", "area_m2"))
  
  area90[[i]] <- ar
  
}

## convert from list to data.table
area90 <- rbindlist(area90)
area90$percent <- 90

area_all <- rbind(area30, area40, area50, area60, area70, area80, area90)

saveRDS(area_all, "output/territories/territory-size.RDS")
