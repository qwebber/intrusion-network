


### Packages ----
libs <- c('data.table','ggplot2', 'gridExtra')
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

col <- c("#f1a340", "#998ec3")

png("figures/FigS10.png", width = 6000, height = 3000, units = "px", res = 600)
FigS10A <- ggplot(all) + 
  geom_point(aes(outstrength_trap, outstrength_behav, 
                 color = grid)) +
  xlab("Outstrength (trapping data)") +
  ylab("Outstrength (behavioural observation data)") +
  scale_color_manual(values = col) +
  ggtitle("A)") +
  ylim(0, 7) +
  xlim(0, 7) +
  theme(
    legend.position = c(0.2, 0.8), 
    legend.key = element_blank(),
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))

FigS10B <- ggplot(all) + 
  geom_point(aes(instrength_trap, instrength_behav, 
                 color = grid)) +
  xlab("Instrength (trapping data)") +
  ylab("Instrength (behavioural observation data)") +
  scale_color_manual(values = col) +
  ggtitle("B)") +
  ylim(0, 1) +
  xlim(0, 1) +
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))

grid.arrange(FigS10A, FigS10B, nrow = 1)
dev.off()
