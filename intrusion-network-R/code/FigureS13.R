


### Packages ----
libs <- c('data.table','dplyr', 'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
metrics <- readRDS("output/metrics.RDS")
metrics <- metrics[gr_year != "2006_KL"]
metrics$squirrel_id <- as.factor(metrics$squirrel_id)
metrics$year <- as.factor(metrics$year)

unique(metrics$gr_year)

metrics <- metrics[!is.na(metrics$sex)]
metrics <- metrics[!is.na(metrics$grid)]

col <- c("#f1a340", "#998ec3")

png("figures/FigS13.png", width = 6000, height = 3000, units = "px", res = 600)
FigS13A <- ggplot(metrics) + 
  geom_point(aes(indegree, instrength, 
                 color= grid)) +
  xlab("Instrength") +
  ylab("Indegree") +
  scale_color_manual(values = col) +
  ggtitle("A)") +
  theme(
    legend.position = c(0.8, 0.8), 
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

FigS13B <- ggplot(metrics) +
  geom_point(aes(outdegree, outstrength, 
                 color= grid)) +
  xlab("Outstrength") +
  ylab("Outdegree") +
  scale_color_manual(values = col) +
  ggtitle("B)") +
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

grid.arrange(FigS13A, FigS13B, nrow = 1)
dev.off()