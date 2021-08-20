


### Packages ----
libs <- c('data.table','ggplot2')
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

areaMean <- all[, median(area_ha), by = c("grid", "year")]
areaMean$CI <- all[, plotrix::std.error(area_ha), by = c("grid", "year")]$V1
areaMean$CI <- areaMean$CI*1.96
areaMean$Study <- "This study"
setnames(areaMean, c("V1", "grid"), c("area", "Grid"))


areaPub <- data.table(Grid = c("Control grid (KL)", "Food supplemented grid" ,"KL and SU", "KL and SU", "KL and SU"),
                      year = c("2008", "2008" ,"2002", "2003", "2004"),
                      area = c(0.18, 0.10, 0.26, 0.38, 0.35), 
                      CI = c(NA, NA, 0.05, 0.05, 0.07),
                      #range = c(, NA, NA, NA),
                      Study = c("Donald and Boutin (2011)", "Donald and Boutin (2011)",
                                "LaMontagne et al. (2013)","LaMontagne et al. (2013)",
                                "LaMontagne et al. (2013)"))

areaMean2 <- rbind(areaMean[year == 2002 | year == 2003 | year == 2004 |
                            year == 2008], areaPub)

col <- c("#8da0cb", "#66c2a5", "#fc8d62")

png("figures/FigS4.png", width = 4000, height = 4000, units = "px", res = 600)
ggplot(areaMean2, aes(year, area)) + 
  geom_errorbar(aes(ymin = area - CI, ymax = area + CI, 
                    color = Study, group = Grid),
                width = 0, 
                size = 0.55,
                position = position_dodge(width = 0.5)) +
  geom_point(aes(color = Study, group = Grid, shape = Grid), 
             position = position_dodge(width = 0.5), 
             size = 2) +
  scale_color_manual(values = col) +
  xlab("") +
  ylab("Estimated territory size (ha)") +
  coord_flip() +
  geom_vline(xintercept = 0.5, linetype = 'dotted') +
  geom_vline(xintercept = 1.5, linetype = 'dotted') +
  geom_vline(xintercept = 2.5, linetype = 'dotted') +
  geom_vline(xintercept = 3.5, linetype = 'dotted') +
  geom_vline(xintercept = 4.5, linetype = 'dotted') +
  geom_vline(xintercept = 5.5, linetype = 'dotted') +
  geom_vline(xintercept = 6.5, linetype = 'dotted') +
  geom_vline(xintercept = 7.5, linetype = 'dotted') +
  theme(#legend.position = 'none',
    legend.title = element_text(size = 14, color = "black"),
    legend.key = element_blank(),
    axis.title = element_text(size = 14, color = 'black'),
    axis.text = element_text(size = 12, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()