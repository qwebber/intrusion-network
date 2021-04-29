


### Packages ----
libs <- c('data.table', 'sf', 'dplyr', 'sp', 'spatsoc',
          'lubridate','ggplot2', 'krsp', 'adehabitatHR',
          'ggforce', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

## prj
prj <- '+init=epsg:26911'

## calculate territory overlap for 50% kernels
terr_over30 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
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

## calculate territory overlap for 50% kernels
terr_over40 <- group_polys(df[gr_year == "KL_2002" | 
                                gr_year == "KL_2004" |
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
terr_overlap_all <- rbind(terr_over40, 
                          terr_over50, terr_over60, 
                          terr_over70, terr_over80, 
                          terr_over90)
terr_overlap_all$year <- as.factor(terr_overlap_all$year)


## Figure
png("figures/FigS3.png", height = 3000, width = 6000, units = "px", res = 600)
aa <- ggplot(terr_overlap_all, aes(as.factor(percent), y =area, fill=year)) +
  geom_point(shape = 21, alpha = 0.3, position = position_jitterdodge()) +
  geom_boxplot(outlier.color = NA, 
               position = position_dodge2(), 
               alpha = 0.25,
               lwd = 0.6,
               color = "black") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("A)") +
  xlab("Kernel density estimator percentage") +
  ylab("Area (ha) of overlap between territories") +
  theme(
    legend.position = c(0.15, 0.8),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1)) 

bb <- ggplot(terr_overlap_all[percent <= 60], aes(as.factor(percent), y =area, fill=year)) +
  geom_point(shape = 21, alpha = 0.3, position = position_jitterdodge()) +
  geom_boxplot(outlier.color = NA, 
               position = position_dodge2(), 
               alpha = 0.25,
               lwd = 0.6,
               color = "black") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("B)") +
  xlab("Kernel density estimator percentage") +
  ylab("Area (ha) of overlap between territories") +
  theme(legend.position = 'none',
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1)) 
grid.arrange(aa,bb, nrow = 1)

dev.off()

