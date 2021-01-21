


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'igraph', 'spatsoc')
lapply(libs, require, character.only = TRUE)

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

trp <- fread("output/trp.csv")

source("functions/GetHRBy.R")


## add column to count number of locs 
trp18 <- trp[gr == "AG" & year == "2018"][,N := .N, by = c("squirrel_id", "gr", "year")]

## generate MCP home ranges
params = c(grid = 400, extent = 3)
grd.mcps <- trp18[N > 6][, GetHRBy(squirrel_id, locXnum, locYnum, 95, type = 'mcp')]

ggplot(grd.mcps, aes(x = long, y = lat, 
                         fill = id)) +
  geom_polygon(alpha = 0.25, size = 0.5, color = "black") +
  coord_equal() 


saveRDS(vert.dt, "output/4-home-range-area.RDS")