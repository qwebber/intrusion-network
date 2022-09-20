

## load packages
library(ggplot2)
library(gridExtra)
library(data.table)
library(glmmTMB)

## load data
terr_overlap_all <- readRDS("output/territories/territory-overlap.RDS")
terr_overlap_all <- terr_overlap_all[year != "2006"]

area_all <- readRDS("output/territories/territory-size.RDS")
area_all$area_ha <- area_all$area_m2/10000
area_all[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

area_all <- area_all[year != "2006"]

## statistics 
a1 <- glmmTMB(area_ha ~ as.factor(percent) + (1|year) + (1|squirrel_id), 
              data = area_all)
summary(a1)

a2 <- glmmTMB(area ~ as.factor(percent) + (1|year) + (1|squirrel_id),
              data = terr_overlap_all)
summary(a2)

## Figure
png("figures/FigS3.png", height = 3000, width = 6000, units = "px", res = 600)

figS3A <- ggplot(area_all[year != "2006"], aes(as.factor(percent), y =area_ha, fill=year)) +
  geom_jitter(shape = 21, alpha = 0.3, position = position_jitterdodge(jitter.width = 0.1)) +
  geom_boxplot(outlier.color = NA, 
               position = position_dodge2(), 
               alpha = 0.25,
               lwd = 0.6,
               color = "black") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("A)") +
  xlab("Kernel density estimator percentage") +
  ylab("Territory area (ha)") +
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
  
figS3B <- ggplot(terr_overlap_all[year != "2006"], aes(as.factor(percent), y =area, fill=year)) +
  geom_jitter(shape = 21, alpha = 0.3, position = position_jitterdodge(jitter.width = 0.2)) +
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
  theme(
    legend.position = 'none',
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

grid.arrange(figS3A,figS3B, nrow = 1)

dev.off()


terr30 <- area_all[percent == 30]
terr30$area_ha30 <- terr30$area_ha
terr50 <- area_all[percent == 50]
terr50$area_ha50 <- terr50$area_ha
terr70 <- area_all[percent == 70]
terr70$area_ha70 <- terr70$area_ha
terr90 <- area_all[percent == 90]
terr90$area_ha90 <- terr90$area_ha

cor.test(terr30$area_ha30, terr50$area_ha50)
cor.test(terr30$area_ha30, terr70$area_ha70)
cor.test(terr30$area_ha30, terr90$area_ha90)

png("figures/FigS4.png", height = 3000, width = 6000, units = "px", res = 600)
FigS4A <- ggplot(cbind(terr30, terr50[,c("area_ha50")])) +
  geom_point(aes(area_ha30, area_ha50, color = year),
             alpha = 0.3) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("A) r = 0.99") +
  xlab("Territory area (ha) using 30% KDE") +
  ylab("Territory area (ha) using 50% KDE") +
  theme(
    legend.position = c(0.2,0.8), 
    #legend.background = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1)) 

FigS4B <- ggplot(cbind(terr30, terr70[,c("area_ha70")])) +
  geom_point(aes(area_ha30, area_ha70, color = year),
             alpha = 0.3) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("B) r = 0.98") +
  xlab("Territory area (ha) using 30% KDE") +
  ylab("Territory area (ha) using 70% KDE") +
  theme(
    legend.position = 'none',
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

FigS4C <- ggplot(cbind(terr30, terr90[,c("area_ha90")])) +
  geom_point(aes(area_ha30, area_ha90, color = year),
             alpha = 0.3) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggtitle("C) r = 0.95") +
  xlab("Territory area (ha) using 30% KDE") +
  ylab("Territory area (ha) using 90% KDE") +
  theme(
    legend.position = 'none',
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

grid.arrange(FigS4A, FigS4B, FigS4C, nrow = 1)
dev.off()
