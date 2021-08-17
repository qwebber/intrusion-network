



## load packages
library(ggplot2)
library(gridExtra)
library(data.table)
library(glmmTMB)

## load data
terr_overlap_all <- readRDS("output/territories/territory-overlap.RDS")
terr_overlap_all <- terr_overlap_all[percent == 50]

png("figures/FigS6.png", height = 3000, width = 3000, units = "px", res = 600)
ggplot(terr_overlap_all, aes(as.factor(year), y =area, fill=year)) +
  geom_jitter(shape = 21, alpha = 0.3, 
              position = position_jitterdodge(jitter.width = 3)) +
  #geom_boxplot(outlier.color = NA, 
  #             position = position_dodge2(), 
  #             alpha = 0,
  #             lwd = 0.6,
  #             color = "black") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
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
dev.off()