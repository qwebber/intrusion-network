


### Packages ----
libs <- c('data.table', 'igraph', 'sp', 'adehabitatHR', 'sf',
          'ggplot2', 'krsp', 'dils', 'spatsoc')
lapply(libs, require, character.only = TRUE)

### load raw data 
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

## check to see temporal changes in data collection
ggplot(df[year == 2005 | year == 2006 | year == 2007][, .N, by = c("squirrel_id","julian", "year")]) +
  geom_point(aes(julian, N, color = as.factor(year))) + 
  theme(legend.position = 'none') +
  facet_wrap(~year)

## check to see temporal changes in trapping vs. behavioural data collection
ggplot(df[, .N, by = c("data", "julian", "year")]) +
  geom_point(aes(julian, N, color = data))  +
  facet_wrap(~year)
## RESULT: 2006 has a large number of behavioural observations in the middle of the summer, 
## but not any more than 2014, 2016, or 2019.

## subset to KL_2006
df06 <- df[gr_year == "KL_2006"]

df06sub <- df06[squirrel_id == 10121 |
                  squirrel_id == 10183 |
                  squirrel_id == 7124 | 
                  squirrel_id == 10317 |
                  squirrel_id == 7092]

ggplot(df06sub) +
   geom_point(aes(locx/30, locy/30, color = squirrel_id)) +
   ylim(-2, 22) + xlim(-12, 22) +
   facet_wrap(~data)

## compare territory size to cone abundance over time
cones <- readRDS("output/auxilliary-data/cones_grids_years.RDS")
setDT(cones)
cones$gr_year <- as.factor(paste(cones$Grid, cones$Year, sep = "_"))
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)
all$area_ha <- all$area_m2/10000


all2 <- merge(all, cones[, c("gr_year", "cone_counts", "cone_index", 
                             "cone_index_t", "mast", "num_trees")], by = "gr_year")

## KL-2006 has very low cone index
ggplot(all2) +
  geom_point(aes(cone_index, area_ha, color = year)) +
  facet_wrap(~grid)

## run KL-2006 territories with 30% KDE
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

## prj
prj <- '+init=epsg:26911'

## parameters for kernel
params = c(grid = 400, extent = 7)

## load home-made functions
source("functions/get_polygon.R")

df2006KL <- df[gr_year == "KL_2006"]

yr2006KL <- data.table(gr_year = as.character(unique(df2006KL$gr_year)))
n2006KL = length(unique(yr2006KL$gr_year))

# Generate territorial polygons ---------------------------------
out_polygon06KL_30 <- get_polygon(input = df2006KL, 
                              n = n2006KL,
                              yr = yr2006KL,
                              in.percent = 30,
                              params = params)

out_polygon06KL_50 <- get_polygon(input = df2006KL, 
                                  n = n2006KL,
                                  yr = yr2006KL,
                                  in.percent = 50,
                                  params = params)


aa <- ggplot(out_polygon06KL_30[[1]]) +
  geom_sf(aes(fill = as.factor(id_polygons)), 
          alpha = 0.5) +
  scale_fill_viridis_d() +
  theme(legend.position = 'none')
bb <- ggplot(out_polygon06KL_50[[1]]) +
  geom_sf(aes(fill = as.factor(id_polygons)), 
          alpha = 0.5) +
  scale_fill_viridis_d() +
  theme(legend.position = 'none')
grid.arrange(aa,bb, nrow = 1)

## calculate territory overlap

## calculate territory overlap for 50% kernels
terr_over50 <- group_polys(df[ gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
                                 gr_year == "KL_2006" |
                                gr_year == "KL_2009" |
                                gr_year == "KL_2014" |
                                gr_year == "KL_2020"]],
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

ggplot(terr_over50) +
  geom_boxplot(aes(year, area))

ar <- data.table(out_polygon06KL[[1]]$id_polygons, out_polygon06KL[[1]]$area)
setnames(ar, c("V1", "V2"), c("squirrel_id", "area30KDE"))

KL06 <- merge(all[gr_year == "KL_2006"], ar, by = "squirrel_id")
KL06$area30KDE_Ha <- KL06$area30KDE/10000

ggplot(KL06) +
  geom_point(aes(area_ha, area30KDE_Ha))

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

edge_list06 <- edge_list[gr_year == "2006_KL"]

edge_list06[, .N, by = c("julian", "intruder_sex")]

ggplot(edge_list06[, .N, by = c("julian", "intruder_sex")]) +
  geom_point(aes(julian, N)) +
  facet_wrap(~intruder_sex)

edge_list06[, .N, by = c("intruder", "intruder_sex")]

ggplot(edge_list06[julian == 150]) +
  geom_jitter(aes(locx/30, locy/30, color = intruder), 
              alpha = 0.25, 
              height = 0.2, width = 0.2) +
  theme(legend.position = 'none')

aa <- edge_list06[julian == 150]
