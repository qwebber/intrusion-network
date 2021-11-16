


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
all <- readRDS("output/final-df.RDS")
all <- all[gr_year != "KL_2006"]
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)
all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

all[, uniqueN(squirrel_id), by = "gr_year"]

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

gr_year <- data.table(id = names(polys))

# Bind
bound <- do.call(rbind, polys)
bound$name <- tstrsplit(rownames(bound), '\\.')[[1]]
bound$grid <- tstrsplit(bound$name, '_')[[1]]
bound$yr <- tstrsplit(bound$name, '_', type.convert = TRUE)[[2]]

bound
bound$id_gr_yr <- paste(bound$id_polygons, bound$name, sep = "_")

#### KL GRIDS ####
## generate annual KL polygons manually
polys1996KL <- sf::st_transform(polys[[1]])
polys1996KL$gr_year <- rep("1996 (n = 21)", length(polys1996KL$id_polygons))
polys1997KL <- sf::st_transform(polys[[5]])
polys1997KL$gr_year <- rep("1997 (n = 22)", length(polys1997KL$id_polygons))
polys1998KL <- sf::st_transform(polys[[2]])
polys1998KL$gr_year <- rep("1998 (n = 0)*", length(polys1998KL$id_polygons))
polys1999KL <- sf::st_transform(polys[[3]])
polys1999KL$gr_year <- rep("1999 (n = 48)", length(polys1999KL$id_polygons))
polys2000KL <- sf::st_transform(polys[[8]])
polys2000KL$gr_year <- rep("2000 (n = 64)", length(polys2000KL$id_polygons))
polys2001KL <- sf::st_transform(polys[[10]])
polys2001KL$gr_year <- rep("2001 (n = 30)", length(polys2001KL$id_polygons))
polys2002KL <- sf::st_transform(polys[[12]])
polys2002KL$gr_year <- rep("2002 (n = 36)", length(polys2002KL$id_polygons))
polys2003KL <- sf::st_transform(polys[[14]])
polys2003KL$gr_year <- rep("2003 (n = 36)", length(polys2003KL$id_polygons))
polys2004KL <- sf::st_transform(polys[[16]])
polys2004KL$gr_year <- rep("2004 (n = 48)", length(polys2004KL$id_polygons))
polys2005KL <- sf::st_transform(polys[[18]])
polys2005KL$gr_year <- rep("2005 (n = 64)*", length(polys2005KL$id_polygons))
polys2006KL <- sf::st_transform(polys[[20]])
polys2006KL$gr_year <- rep("2006 (n = 82)", length(polys2006KL$id_polygons))
polys2007KL <- sf::st_transform(polys[[22]])
polys2007KL$gr_year <- rep("2007 (n = 40)", length(polys2007KL$id_polygons))
polys2008KL <- sf::st_transform(polys[[24]])
polys2008KL$gr_year <- rep("2008 (n = 66)", length(polys2008KL$id_polygons))
polys2009KL <- sf::st_transform(polys[[26]])
polys2009KL$gr_year <- rep("2009 (n = 37)", length(polys2009KL$id_polygons))
polys2010KL <- sf::st_transform(polys[[28]])
polys2010KL$gr_year <- rep("2010 (n = 38)*", length(polys2010KL$id_polygons))
polys2011KL <- sf::st_transform(polys[[30]])
polys2011KL$gr_year <- rep("2011 (n = 38)", length(polys2011KL$id_polygons))
polys2012KL <- sf::st_transform(polys[[32]])
polys2012KL$gr_year <- rep("2012 (n = 47)", length(polys2012KL$id_polygons))
polys2013KL <- sf::st_transform(polys[[34]])
polys2013KL$gr_year <- rep("2013 (n = 77)", length(polys2013KL$id_polygons))
polys2014KL <- sf::st_transform(polys[[36]])
polys2014KL$gr_year <- rep("2014 (n = 94)*", length(polys2014KL$id_polygons))
polys2015KL <- sf::st_transform(polys[[38]])
polys2015KL$gr_year <- rep("2015 (n = 54)", length(polys2015KL$id_polygons))
polys2016KL <- sf::st_transform(polys[[39]])
polys2016KL$gr_year <- rep("2016 (n = 56)", length(polys2016KL$id_polygons))
polys2017KL <- sf::st_transform(polys[[41]])
polys2017KL$gr_year <- rep("2017 (n = 73)", length(polys2017KL$id_polygons))
polys2018KL <- sf::st_transform(polys[[43]])
polys2018KL$gr_year <- rep("2018 (n = 77)", length(polys2018KL$id_polygons))
polys2019KL <- sf::st_transform(polys[[45]])
polys2019KL$gr_year <- rep("2019 (n = 83)*", length(polys2019KL$id_polygons))
polys2020KL <- sf::st_transform(polys[[47]])
polys2020KL$gr_year <- rep("2020 (n = 25)", length(polys2020KL$id_polygons))

polys_KL <- rbind(polys1996KL, polys1997KL, polys1998KL, polys1999KL, 
                  polys2000KL, polys2001KL, polys2002KL, 
                   polys2003KL, polys2004KL, polys2005KL,
                   polys2006KL, polys2007KL, polys2008KL,
                   polys2009KL, polys2010KL, polys2011KL,
                   polys2012KL, polys2013KL, polys2014KL,
                   polys2015KL, polys2016KL, polys2017KL,
                   polys2018KL, polys2019KL, polys2020KL)

png("figures/FigS5.png", width = 6000, height = 6000, units = "px", res = 600)
ggplot() +
  geom_sf(data = polys_KL, 
          aes(fill = id_polygons), 
          alpha = 0.5) +
  geom_sf(data = subset(polys_KL, gr_year == "1998 (n = 0)*"),
          aes(fill = id_polygons),
          fill = "white", color = "white") +
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
  facet_wrap(~gr_year, nrow = 5)
dev.off()  



## visual check to see if any individuals have two kernels

## SU2016: 43 individuals, 45 polygons 
## (squirrel_id == 12026 & squirrel_id == 13132 & squirrel_id == 20060)
ggplot() +
  geom_sf(data = polys$SU_2016[5,], 
          aes(fill = id_polygons), 
          alpha = 0.5)
ggplot() +
  geom_sf(data = polys$SU_2016[13,],  ## 13, 24
          aes(fill = id_polygons), 
          alpha = 0.5)

## KL2016: 56 individuals, 58 polygons (squirrel_id == 11256 & squirrel_id == 19649)
ggplot() +
  geom_sf(data = polys$KL_2016[1,], 
          aes(fill = id_polygons), 
          alpha = 0.5)

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

edge_list <- readRDS("output/edge_list.RDS")
edge_list2 <- edge_list[owner == 19649 & gr_year == "KL_2016"]


ggplot() +
  geom_sf(data = polys$KL_2016[14,], 
          aes(fill = id_polygons), 
          alpha = 0.5) +
  geom_point(data = df[squirrel_id == 19649 & gr_year == "KL_2016"], 
             aes(locx, locy),
             alpha = 0.75) +
  geom_point(data = census_all[squirrel_id == 19649 & gr_year == "KL_2016"], 
             aes(locx, locy,), #shape = 23, size = 5, fill = "blue", color = "blue", 
             alpha = 0.75)  +
  geom_point(data = edge_list2, 
             aes(locx, locy, color = edge),
             alpha = 0.75)  +
  theme(#legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


## SU2010: 17 individuals, 18 polygons (squirrel_id == 10325)
ggplot() +
     geom_sf(data = polys$SU_2010[3,], 
                aes(fill = id_polygons), 
                alpha = 0.5)
## KL2005: 64 individuals, 65 polygons (squirrel_id == 6353)
ggplot() +
  geom_sf(data = polys$KL_2005[7,], 
          aes(fill = id_polygons), 
          alpha = 0.5)

  
#### SU GRIDS ####

## generate annual KL polygons manually
polys1996SU <- sf::st_transform(polys[[4]])
polys1996SU$gr_year <- rep("1996 (n = 15)", length(polys1996SU$id_polygons))
polys1997SU <- sf::st_transform(polys[[6]])
polys1997SU$gr_year <- rep("1997 (n = 13)", length(polys1997SU$id_polygons))
polys1998SU <- sf::st_transform(polys[[2]])
polys1998SU$gr_year <- rep("1998 (n = 13)*", length(polys1998SU$id_polygons))
polys1999SU <- sf::st_transform(polys[[7]])
polys1999SU$gr_year <- rep("1999 (n = 13)", length(polys1999SU$id_polygons))
polys2000SU <- sf::st_transform(polys[[9]])
polys2000SU$gr_year <- rep("2000 (n = 35)", length(polys2000SU$id_polygons))
polys2001SU <- sf::st_transform(polys[[11]])
polys2001SU$gr_year <- rep("2001 (n = 19)", length(polys2001SU$id_polygons))
polys2002SU <- sf::st_transform(polys[[13]])
polys2002SU$gr_year <- rep("2002 (n = 32)", length(polys2002SU$id_polygons))
polys2003SU <- sf::st_transform(polys[[15]])
polys2003SU$gr_year <- rep("2003 (n = 40)", length(polys2003SU$id_polygons))
polys2004SU <- sf::st_transform(polys[[17]])
polys2004SU$gr_year <- rep("2004 (n = 53)", length(polys2004SU$id_polygons))
polys2005SU <- sf::st_transform(polys[[19]])
polys2005SU$gr_year <- rep("2005 (n = 73)*", length(polys2005SU$id_polygons))
polys2006SU <- sf::st_transform(polys[[21]])
polys2006SU$gr_year <- rep("2006 (n = 64)", length(polys2006SU$id_polygons))
polys2007SU <- sf::st_transform(polys[[23]])
polys2007SU$gr_year <- rep("2007 (n = 31)", length(polys2007SU$id_polygons))
polys2008SU <- sf::st_transform(polys[[25]])
polys2008SU$gr_year <- rep("2008 (n = 27)", length(polys2008SU$id_polygons))
polys2009SU <- sf::st_transform(polys[[27]])
polys2009SU$gr_year <- rep("2009 (n = 29)", length(polys2009SU$id_polygons))
polys2010SU <- sf::st_transform(polys[[29]])
polys2010SU$gr_year <- rep("2010 (n = 17)*", length(polys2010SU$id_polygons))
polys2011SU <- sf::st_transform(polys[[31]])
polys2011SU$gr_year <- rep("2011 (n = 16)", length(polys2011SU$id_polygons))
polys2012SU <- sf::st_transform(polys[[33]])
polys2012SU$gr_year <- rep("2012 (n = 16)", length(polys2012SU$id_polygons))
polys2013SU <- sf::st_transform(polys[[35]])
polys2013SU$gr_year <- rep("2013 (n = 32)", length(polys2013SU$id_polygons))
polys2014SU <- sf::st_transform(polys[[37]])
polys2014SU$gr_year <- rep("2014 (n = 49)*", length(polys2014SU$id_polygons))
polys2015SU <- sf::st_transform(polys[[38]])
polys2015SU$gr_year <- rep("2015 (n = 0)", length(polys2015SU$id_polygons))
polys2016SU <- sf::st_transform(polys[[40]])
polys2016SU$gr_year <- rep("2016 (n = 43)", length(polys2016SU$id_polygons))
polys2017SU <- sf::st_transform(polys[[42]])
polys2017SU$gr_year <- rep("2017 (n = 17)", length(polys2017SU$id_polygons))
polys2018SU <- sf::st_transform(polys[[44]])
polys2018SU$gr_year <- rep("2018 (n = 15)", length(polys2018SU$id_polygons))
polys2019SU <- sf::st_transform(polys[[46]])
polys2019SU$gr_year <- rep("2019 (n = 32)*", length(polys2019SU$id_polygons))
polys2020SU <- sf::st_transform(polys[[47]])
polys2020SU$gr_year <- rep("2020 (n = 0)", length(polys2020SU$id_polygons))

polys_SU <- rbind(polys1996SU, polys1997SU, polys1998SU, polys1999SU,
                  polys2000SU, polys2001SU, polys2002SU, polys2003SU, 
                  polys2004SU, polys2005SU, polys2006SU,
                  polys2007SU, polys2008SU, polys2009SU,
                  polys2010SU, polys2011SU, polys2012SU, polys2013SU,
                  polys2014SU, polys2015SU, polys2016SU, polys2017SU,
                  polys2018SU, polys2019SU, polys2020SU)

png("figures/FigS6.png", width = 6000, height = 6000, units = "px", res = 600)
ggplot(data = polys_SU) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  geom_sf(data = subset(polys_SU, gr_year == "2020 (n = 0)"),
          aes(fill = id_polygons),
          fill = "white", color = "white") +
  geom_sf(data = subset(polys_SU, gr_year == "2015 (n = 0)"),
          aes(fill = id_polygons),
          fill = "white", color = "white") +
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
  facet_wrap(~gr_year, nrow = 5)
dev.off()  



## check if multiple kernels

yr <- data.table(gr_year = unique(all$gr_year))
yr <- yr[order(yr$gr_year), ]
Npolys <- data.table(year = yr$gr_year,
           N = c(dim(polys1996KL)[1], dim(polys1997KL)[1],
                 dim(polys1999KL)[1], dim(polys2000KL)[1],
                 dim(polys2001KL)[1], dim(polys2002KL)[1],
                 dim(polys2003KL)[1], dim(polys2004KL)[1],
                 dim(polys2005KL)[1], #dim(polys2006KL)[1],
                 dim(polys2007KL)[1], dim(polys2008KL)[1],
                 dim(polys2009KL)[1], dim(polys2010KL)[1],
                 dim(polys2011KL)[1], dim(polys2012KL)[1],
                 dim(polys2013KL)[1], dim(polys2014KL)[1],
                 dim(polys2015KL)[1], dim(polys2016KL)[1],
                 dim(polys2017KL)[1], dim(polys2018KL)[1],
                 dim(polys2019KL)[1], dim(polys2020KL)[1],
                 dim(polys1996SU)[1], dim(polys1997SU)[1], 
                 dim(polys1998SU)[1],
                 dim(polys1999SU)[1], dim(polys2000SU)[1],
                 dim(polys2001SU)[1], dim(polys2002SU)[1],
                 dim(polys2003SU)[1], dim(polys2004SU)[1],
                 dim(polys2005SU)[1], dim(polys2006SU)[1],
                 dim(polys2007SU)[1], dim(polys2008SU)[1],
                 dim(polys2009SU)[1], dim(polys2010SU)[1],
                 dim(polys2011SU)[1], dim(polys2012SU)[1],
                 dim(polys2013SU)[1], dim(polys2014SU)[1],#dim(polys2015SU)[1], 
                 dim(polys2016SU)[1], dim(polys2017SU)[1], 
                 dim(polys2018SU)[1], dim(polys2019SU)[1]), #dim(polys2020SU)[1]),
           polys =  c(dim(st_cast(polys1996KL, 'POLYGON'))[1], dim(st_cast(polys1997KL, 'POLYGON'))[1],
                      dim(st_cast(polys1999KL, 'POLYGON'))[1], dim(st_cast(polys2000KL, 'POLYGON'))[1],
                      dim(st_cast(polys2001KL, 'POLYGON'))[1], dim(st_cast(polys2002KL, 'POLYGON'))[1],
                      dim(st_cast(polys2003KL, 'POLYGON'))[1], dim(st_cast(polys2004KL, 'POLYGON'))[1],
                      dim(st_cast(polys2005KL, 'POLYGON'))[1], #dim(st_cast(polys2006KL, 'POLYGON'))[1],
                      dim(st_cast(polys2007KL, 'POLYGON'))[1], dim(st_cast(polys2008KL, 'POLYGON'))[1],
                      dim(st_cast(polys2009KL, 'POLYGON'))[1], dim(st_cast(polys2010KL, 'POLYGON'))[1],
                      dim(st_cast(polys2011KL, 'POLYGON'))[1], dim(st_cast(polys2012KL, 'POLYGON'))[1],
                      dim(st_cast(polys2013KL, 'POLYGON'))[1], dim(st_cast(polys2014KL, 'POLYGON'))[1],
                      dim(st_cast(polys2015KL, 'POLYGON'))[1], dim(st_cast(polys2016KL, 'POLYGON'))[1],
                      dim(st_cast(polys2017KL, 'POLYGON'))[1], dim(st_cast(polys2018KL, 'POLYGON'))[1],
                      dim(st_cast(polys2019KL, 'POLYGON'))[1], dim(st_cast(polys2020KL, 'POLYGON'))[1],
                      dim(st_cast(polys1996SU, 'POLYGON'))[1], dim(st_cast(polys1997SU, 'POLYGON'))[1],
                      dim(st_cast(polys1998SU, 'POLYGON'))[1],
                      dim(st_cast(polys1999SU, 'POLYGON'))[1], dim(st_cast(polys2000SU, 'POLYGON'))[1],
                      dim(st_cast(polys2001SU, 'POLYGON'))[1], dim(st_cast(polys2002SU, 'POLYGON'))[1],
                      dim(st_cast(polys2003SU, 'POLYGON'))[1], dim(st_cast(polys2004SU, 'POLYGON'))[1],
                      dim(st_cast(polys2005SU, 'POLYGON'))[1], dim(st_cast(polys2006SU, 'POLYGON'))[1],
                      dim(st_cast(polys2007SU, 'POLYGON'))[1], dim(st_cast(polys2008SU, 'POLYGON'))[1],
                      dim(st_cast(polys2009SU, 'POLYGON'))[1], dim(st_cast(polys2010SU, 'POLYGON'))[1],
                      dim(st_cast(polys2011SU, 'POLYGON'))[1], dim(st_cast(polys2012SU, 'POLYGON'))[1],
                      dim(st_cast(polys2013SU, 'POLYGON'))[1], dim(st_cast(polys2014SU, 'POLYGON'))[1], #dim(st_cast(polys2015SU, 'POLYGON'))[1], 
                      dim(st_cast(polys2016SU, 'POLYGON'))[1], dim(st_cast(polys2017SU, 'POLYGON'))[1], 
                      dim(st_cast(polys2018SU, 'POLYGON'))[1], dim(st_cast(polys2019SU, 'POLYGON'))[1])) #, dim(st_cast(polys2020SU, 'POLYGON'))[1]))

## calculate number of individuals with multiple polygons
Npolys$delta <- Npolys$polys - Npolys$N

## calculate proportion of grid occupied by territory

out <- c()
for(i in 1:length(yr$gr_year)) {

  temp_poly <- st_union(polys[[i]])

  ar <- as.numeric(st_area(temp_poly))
  
  df <- data.table(gr_year = yr$gr_year[[i]],
                         area = ar)

  out[[i]] <- df
  
  }

out2 <- rbindlist(out)
out2$area_ha <- out2$ar/10000
out2$area_used <- out2$area_ha/40
out2[, c("Grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]


col <- c("#f1a340", "#998ec3")

ggplot(out2, aes(year, area_used, color = Grid, group = Grid)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  ylab("Proportion of grid occupied by territories") +
  scale_color_manual(values = col) +
  geom_hline(aes(yintercept = 1), lty = 1) + 
  geom_vline(aes(xintercept = "1993"), lty = 2) + # 1993
  geom_vline(aes(xintercept = "1998"), lty = 2) + # 1998
  geom_vline(aes(xintercept = "2005"), lty = 2) + # 2005
  geom_vline(aes(xintercept = "2010"), lty = 2) + # 2010
  geom_vline(aes(xintercept = "2014"), lty = 2) + # 2014
  geom_vline(aes(xintercept = "2019"), lty = 2) + # 2019
  theme(
    legend.position = c(0.2, 0.9),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))


ggplot(st_union(polys$KL_2017)) +
  geom_sf() +
  ggtitle("KL 2017 combined polygon") +
  theme(
    legend.position = c(0.2, 0.9),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))
