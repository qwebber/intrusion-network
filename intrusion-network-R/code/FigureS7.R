
### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

col <- c("#f1a340", "#998ec3")

setnames(edge_list, "grid", "Grid")

png("figures/FigS7.png", height = 6000, width = 6000, units = "px", res = 600)
ggplot(edge_list[edge == 1][, .N, by = c("julian", "year", "Grid", "gr_year")],
       aes(julian, N)) +
  geom_point(aes(color = Grid),
             alpha = 0.5) +
  geom_smooth(method = "loess", color = "black") +
  scale_color_manual(values = col) +
  facet_wrap(~year, scale = "free") +
  xlim(70, 250) +
  ylab("Frequency of intrusions") +
  xlab("Day of the year") +
  theme(
    legend.key = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 14, color = "black"), 
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(size = 12, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))
dev.off()
