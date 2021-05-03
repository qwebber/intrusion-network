

## load packages
library(ggplot2)
library(gridExtra)
library(data.table)

## load data
terr_overlap_all <- readRDS("output/territories/territory-overlap.RDS")
terr_overlap_all <- terr_overlap_all[percent > 40]

area_all <- readRDS("output/territories/territory-size.RDS")
area_all$area_ha <- area_all$area_m2/10000
area_all[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)][,c("gr_year") := NULL]

area_all <- area_all[percent > 40]

## statistics 
a1 <- glmmTMB(area_ha ~ as.factor(percent) + (1|year) + (1|squirrel_id), 
              data = area_all)
summary(a1)

a2 <- glmmTMB(area ~ as.factor(percent) + (1|year) + (1|squirrel_id) + (1|squirrel_id2), 
              data = terr_overlap_all)
summary(a2)

## Figure
png("figures/FigS3.png", height = 3000, width = 6000, units = "px", res = 600)

aa <- ggplot(area_all, aes(as.factor(percent), y =area_ha, fill=year)) +
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
  
bb <- ggplot(terr_overlap_all, aes(as.factor(percent), y =area, fill=year)) +
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

grid.arrange(aa,bb, nrow = 1)

dev.off()
