


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'igraph', 'spatsoc',
          'ggplot2')
lapply(libs, require, character.only = TRUE)

trp <- fread("output/trp.csv")
lifetime_clean <- fread("output/lifetime_clean.csv")

source("functions/GetHRBy.R")

## birth year df
byear <- lifetime_clean[,c("squirrel_id", "grid", "byear")]

## add column to count number of locs 
trp16 <- trp[gr == "AG" & year == "2016"][,N := .N, by = c("squirrel_id", "gr", "year")]

## add byear to trp data
merge(trp16, byear, by = "squirrel_id")

## remove NAs from coords
trp16 <- trp16[!is.na(locYnum)]
trp16 <- trp16[!is.na(locXnum)]

## generate MCP home ranges
params = c(grid = 400, extent = 3)
grd.mcps <- trp16[N > 6][, GetHRBy(squirrel_id, locXnum, locYnum, 95, type = 'mcp')]

## visualize MCPs
ggplot(grd.mcps, aes(x = long, y = lat, 
                         fill = id)) +
  geom_polygon(alpha = 0.25, size = 0.5, color = "black") +
  coord_equal() 


saveRDS(vert.dt, "output/4-home-range-area.RDS")