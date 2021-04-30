

library(data.table)
library(ggplot2)

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")
KL_2005 <- sf::st_transform(polys$KL_2005)
KL_2005$gr_year <- rep("2005", length(KL_2005$id_polygons))

KL_2006 <- sf::st_transform(polys$KL_2006)
KL_2006$gr_year <- rep("2006", length(KL_2006$id_polygons))

## subset point data
sq <- df[squirrel_id == 7390 |
           squirrel_id == 7106 |
           squirrel_id == 8727 |
           squirrel_id == 6868 |
           squirrel_id == 10138] 

## subset polygons
polys05 <- subset(KL_2005, id_polygons == 7390 |
                    id_polygons == 7106 |
                    id_polygons == 8727 |
                    id_polygons == 6868 |
                    id_polygons == 10138)
polys06 <- subset(KL_2006, id_polygons == 7390 |
                    id_polygons == 7106 |
                    id_polygons == 8727 |
                    id_polygons == 6868 |
                    id_polygons == 10138)

polys_all <- rbind(polys05, polys06)
polys_all$year <- polys_all$gr_year

png("figures/FigS6.png", width = 6000, height = 3000, units = "px", res = 600)
ggplot(data = polys_all) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  coord_sf(datum = st_crs(32648)) +
  geom_jitter(data = sq[year == "2005" | 
                        year == "2006"], aes(locx, locy, color = factor(squirrel_id)), 
              alpha = 0.5) + 
  ylab("") + xlab("") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(#legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  facet_wrap(~year)
dev.off()

