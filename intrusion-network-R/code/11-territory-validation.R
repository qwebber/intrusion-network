

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
source("functions/GetHRBy.R")

## load SPDF file
## save SPDF
spdf <- readRDS("output/edge_list_data/spdf.RDS")

## subset to KL 2015 and 2016
spdf2 <- list(spdf[[22]])

yr2 <- data.table(gr_year = as.factor(c("KL_2016")))

## parameters for kernel
params = c(grid = 700, extent = 4)

a1 <- df[gr_year == "KL_2018"]

a2 <- a1[N > 50]
a2 <- plyr::ddply(a2, c('squirrel_id'))
a2 <- setDT(a2)[, rowAll := seq_len(.N), by = c("squirrel_id")]

a3 <- a2[rowAll < 51]

out <- c()
for(i in 1:2){ 

  df_sub <- a3[rowAll < 51 - i]
  
  a2 <- df_sub[, GetHRBy(squirrel_id, ## id must be squirrel_id
                        locx, locy, ## coords must be locx and locy
                        in.percent = 50, 
                        params = params,
                        type = "kernel")]
  
  a3 <- data.table(id = a2$id, 
                   area = a2$area,
                   iter = i)
  
  out[[i]] <- a3

}


# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(df = df[gr_year == "KL_2016"], 
                           n = yr2$gr_year,
                           in.percent = 50,
                           params = params)

range(df[gr_year == "KL_2015" | gr_year == "KL_2016"][, .N, by = "squirrel_id"]$N)
