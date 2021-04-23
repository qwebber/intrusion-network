


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")


polys05 <- polys$KL_2005

## prj
prj <- '+init=epsg:26911'

out_spdf <- readRDS("output/edge-list-inputs/spdf.RDS")
  
xy <- get_spdf(df = df[,c("julian","locx", "locy", "squirrel_id", "gr_year")], 
                     n = n,
                     yr = yr)

    KOver = kerneloverlap(out_spdf$KL_2005,
                                        method = "HR",
                                        percent = 50,
                                        grid = 700)
    
    KOver <- as.matrix(KOver)
    diag(KOver) <- NA
    KOver[lower.tri(KOver)] <- NA


source("functions/hr_network.R")

hr.nets <- hr_network(df[gr_year == "KL_2005"], 
                      id = 'squirrel_id', utm = prj, 
                      #by = c('season', 'HERD', 'Year'),
                      returns = 'overlap')
