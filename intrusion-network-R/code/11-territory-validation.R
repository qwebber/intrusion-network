

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

a3[, sample(nrow(.SD), 10), by = "squirrel_id"]
a4 <- a3[,.SD[sample(.N, min(10,.N))],by = "squirrel_id"]


out <- c()
for(i in 1:36){ 

  samp <- 51 - i
  
  df_sub <- a3[,.SD[sample(.N, min(samp,.N))],by = "squirrel_id"]
  
  hrs <- df_sub[, GetHRBy(squirrel_id, ## id must be squirrel_id
                        locx, locy, ## coords must be locx and locy
                        in.percent = 50, 
                        params = params,
                        type = "kernel")]
  
  hrsDT <- data.table(id = hrs$id, 
                   area = hrs$area,
                   iter = 51 - i)
  
  out[[i]] <- hrsDT

}

saveRDS(out,"output/territories/KL2018-50-pts.RDS")

out2 <- rbindlist(out)

out2$iter <- as.numeric(out2$iter)

out2$area50 <- rep(out2[iter == 50]$area, 36)

out2$propArea <- out2$area/out2$area50

out3 <- out2[, mean(propArea), by = "iter"]
out3$lower <- out2[, gmodels::ci(propArea)[2], by = "iter"]$V1
out3$upper <- out2[, gmodels::ci(propArea)[3], by = "iter"]$V1


ggplot(out3) +
  geom_ribbon(aes(iter, V1, ymin = lower, ymax = upper), fill = "lightgrey") +
  geom_line(aes(iter, V1)) +
  ylab("Area of territory with n locs/area of territory with 50 locs") +
  xlab("Number of locs") +
  theme(legend.position = 'none')

# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(df = df[gr_year == "KL_2016"], 
                           n = yr2$gr_year,
                           in.percent = 50,
                           params = params)

range(df[gr_year == "KL_2015" | gr_year == "KL_2016"][, .N, by = "squirrel_id"]$N)
