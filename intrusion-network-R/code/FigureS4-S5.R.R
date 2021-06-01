


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

gr_year <- data.table(id = names(polys))


#### KL GRIDS ####

## generate annual KL polygons manually
polys1996 <- sf::st_transform(polys[[1]])
polys1996$gr_year <- rep("1996", length(polys1996$id_polygons))
polys1997 <- sf::st_transform(polys[[5]])
polys1997$gr_year <- rep("1997", length(polys1997$id_polygons))
polys1999 <- sf::st_transform(polys[[3]])
polys1999$gr_year <- rep("1999", length(polys1999$id_polygons))
polys2000 <- sf::st_transform(polys[[8]])
polys2000$gr_year <- rep("2000", length(polys2000$id_polygons))
polys2001 <- sf::st_transform(polys[[10]])
polys2001$gr_year <- rep("2001", length(polys2001$id_polygons))
polys2002 <- sf::st_transform(polys[[12]])
polys2002$gr_year <- rep("2002", length(polys2002$id_polygons))
polys2003 <- sf::st_transform(polys[[14]])
polys2003$gr_year <- rep("2003", length(polys2003$id_polygons))
polys2004 <- sf::st_transform(polys[[16]])
polys2004$gr_year <- rep("2004", length(polys2004$id_polygons))
polys2005 <- sf::st_transform(polys[[18]])
polys2005$gr_year <- rep("2005", length(polys2005$id_polygons))
polys2006 <- sf::st_transform(polys[[20]])
polys2006$gr_year <- rep("2006", length(polys2006$id_polygons))
polys2007 <- sf::st_transform(polys[[22]])
polys2007$gr_year <- rep("2007", length(polys2007$id_polygons))
polys2008 <- sf::st_transform(polys[[24]])
polys2008$gr_year <- rep("2008", length(polys2008$id_polygons))
polys2009 <- sf::st_transform(polys[[26]])
polys2009$gr_year <- rep("2009", length(polys2009$id_polygons))
polys2010 <- sf::st_transform(polys[[28]])
polys2010$gr_year <- rep("2010", length(polys2010$id_polygons))
polys2011 <- sf::st_transform(polys[[30]])
polys2011$gr_year <- rep("2011", length(polys2011$id_polygons))
polys2012 <- sf::st_transform(polys[[32]])
polys2012$gr_year <- rep("2012", length(polys2012$id_polygons))
polys2013 <- sf::st_transform(polys[[34]])
polys2013$gr_year <- rep("2013", length(polys2013$id_polygons))
polys2014 <- sf::st_transform(polys[[36]])
polys2014$gr_year <- rep("2014", length(polys2014$id_polygons))
polys2015 <- sf::st_transform(polys[[38]])
polys2015$gr_year <- rep("2015", length(polys2015$id_polygons))
polys2016 <- sf::st_transform(polys[[39]])
polys2016$gr_year <- rep("2016", length(polys2016$id_polygons))
polys2017 <- sf::st_transform(polys[[41]])
polys2017$gr_year <- rep("2017", length(polys2017$id_polygons))
polys2018 <- sf::st_transform(polys[[43]])
polys2018$gr_year <- rep("2018", length(polys2018$id_polygons))
polys2019 <- sf::st_transform(polys[[45]])
polys2019$gr_year <- rep("2019", length(polys2019$id_polygons))
polys2020 <- sf::st_transform(polys[[47]])
polys2020$gr_year <- rep("2020", length(polys2020$id_polygons))

polys_KL <- rbind(polys1996, polys1997, polys1999, 
                  polys2000, polys2001, polys2002, 
                   polys2003, polys2004, polys2005,
                   polys2006, polys2007, polys2008,
                   polys2009, polys2010, polys2011,
                   polys2012, polys2013, polys2014,
                   polys2015, polys2016, polys2017,
                   polys2018, polys2019, polys2020)

png("figures/FigS4.png", width = 6000, height = 6000, units = "px", res = 600)
ggplot(data = polys_KL) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  #xlim(-10, 25) + 
  #ylim(-3, 25) +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~gr_year, nrow = 4)
dev.off()  



#### SU GRIDS ####

## generate annual KL polygons manually
polys1996 <- sf::st_transform(polys[[4]])
polys1996$gr_year <- rep("1996", length(polys1996$id_polygons))
polys1997 <- sf::st_transform(polys[[6]])
polys1997$gr_year <- rep("1997", length(polys1997$id_polygons))
polys1998 <- sf::st_transform(polys[[2]])
polys1998$gr_year <- rep("1998", length(polys1998$id_polygons))
polys1999 <- sf::st_transform(polys[[7]])
polys1999$gr_year <- rep("1999", length(polys1999$id_polygons))
polys2000 <- sf::st_transform(polys[[9]])
polys2000$gr_year <- rep("2000", length(polys2000$id_polygons))
polys2001 <- sf::st_transform(polys[[11]])
polys2001$gr_year <- rep("2001", length(polys2001$id_polygons))
polys2002 <- sf::st_transform(polys[[13]])
polys2002$gr_year <- rep("2002", length(polys2002$id_polygons))
polys2003 <- sf::st_transform(polys[[15]])
polys2003$gr_year <- rep("2003", length(polys2003$id_polygons))
polys2004 <- sf::st_transform(polys[[17]])
polys2004$gr_year <- rep("2004", length(polys2004$id_polygons))
polys2005 <- sf::st_transform(polys[[19]])
polys2005$gr_year <- rep("2005", length(polys2005$id_polygons))
polys2006 <- sf::st_transform(polys[[21]])
polys2006$gr_year <- rep("2006", length(polys2006$id_polygons))
polys2007 <- sf::st_transform(polys[[23]])
polys2007$gr_year <- rep("2007", length(polys2007$id_polygons))
polys2008 <- sf::st_transform(polys[[25]])
polys2008$gr_year <- rep("2008", length(polys2008$id_polygons))
polys2009 <- sf::st_transform(polys[[27]])
polys2009$gr_year <- rep("2009", length(polys2009$id_polygons))
polys2010 <- sf::st_transform(polys[[29]])
polys2010$gr_year <- rep("2010", length(polys2010$id_polygons))
polys2011 <- sf::st_transform(polys[[31]])
polys2011$gr_year <- rep("2011", length(polys2011$id_polygons))
polys2012 <- sf::st_transform(polys[[33]])
polys2012$gr_year <- rep("2012", length(polys2012$id_polygons))
polys2013 <- sf::st_transform(polys[[35]])
polys2013$gr_year <- rep("2013", length(polys2013$id_polygons))
polys2014 <- sf::st_transform(polys[[37]])
polys2014$gr_year <- rep("2014", length(polys2014$id_polygons))
polys2016 <- sf::st_transform(polys[[40]])
polys2016$gr_year <- rep("2016", length(polys2016$id_polygons))
polys2017 <- sf::st_transform(polys[[42]])
polys2017$gr_year <- rep("2017", length(polys2017$id_polygons))
polys2018 <- sf::st_transform(polys[[44]])
polys2018$gr_year <- rep("2018", length(polys2018$id_polygons))
polys2019 <- sf::st_transform(polys[[46]])
polys2019$gr_year <- rep("2019", length(polys2019$id_polygons))



polys_SU <- rbind(polys1996, polys1997, polys1998, polys1999,
                  polys2000, polys2001, polys2002, polys2003, 
                  polys2004, polys2005, polys2006,
                  polys2007, polys2008, polys2009,
                  polys2010, polys2011, polys2012, polys2013,
                  polys2014, polys2016, polys2017,
                  polys2018, polys2019)

png("figures/FigS5.png", width = 6000, height = 6000, units = "px", res = 600)
ggplot(data = polys_SU) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  #xlim(-10, 25) + 
  #ylim(-3, 25) +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~gr_year, nrow = 4)
dev.off()  
