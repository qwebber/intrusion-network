
### Packages ----
libs <- c('data.table',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
density <- readRDS("output/auxilliary-data/spring-density.RDS")
#density <- setDT(density)[year > 1995] 
density <- setDT(density)[grid == "KL" | grid == "SU"]


png("figures/Fig3.png", width = 5000, height = 5000, units = "px", res = 600)
ggplot(data = density,
       aes(as.factor(year), spr_density, group = grid, color = grid)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("orange", "royalblue")) +
  geom_line() +
  ylab("Spring density (squirrels/ha)") +
  xlab("Year") +
  ylim(0,4.5) +
  geom_vline(aes(xintercept = "1993"), lty = 2, alpha = 0.5) + # 1998
  geom_vline(aes(xintercept = "1998"), lty = 2, alpha = 0.5) + # 1998
  geom_vline(aes(xintercept = "2005"), lty = 2, alpha = 0.5) + # 2005
  geom_vline(aes(xintercept = "2010"), lty = 2, alpha = 0.5) + # 2010
  geom_vline(aes(xintercept = "2014"), lty = 2, alpha = 0.5) + # 2014
  geom_vline(aes(xintercept = "2019"), lty = 2, alpha = 0.5) + # 2019
  theme(
    legend.position = c(0.05, 0.85),
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
dev.off()
