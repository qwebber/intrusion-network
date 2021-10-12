


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

df[, .N, by = "year"]

dd <- df[, .N, by = c("gr_year", "data", "year")]
## load annual density
density <- readRDS("output/auxilliary-data/spring-density.RDS")
density$gr_year <- as.factor(paste(density$grid, density$year, sep = "_"))

all <- merge(dd[, c("year") := NULL], density, by = "gr_year")


col <- c("#f1a340", "#998ec3")

png("figures/FigS12.png", width = 4000, height = 4000, units = "px", res = 600)
FigS12A <- ggplot(all[data == "behaviour"]) +
  geom_point(aes(spr_density, N, color = grid), size = 2) +
  ylab("Number of behavioural observations") +
  xlab("Spring density (squirrels/ha)") +
  ggtitle('A)') +
  scale_color_manual(values = col) +
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

FigS12B <- ggplot(all[data == "trap"]) +
  geom_point(aes(spr_density, N, color = grid), size = 2) +
  ylab("Number of trapping events") +
  xlab("Spring density (squirrels/ha)") +
  ggtitle('B)') +
  scale_color_manual(values = col) +
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

FigS12C <- ggplot(all[data == "behaviour"]) +
  geom_point(aes(year, N, color = grid), size = 2) +
  ylab("Number of behavioural observations") +
  xlab("Year") +
  ggtitle('C)') +
  scale_color_manual(values = col) +
  geom_vline(aes(xintercept = 1998), lty = 2) + # 1998
  geom_vline(aes(xintercept = 2005), lty = 2) + # 2005
  geom_vline(aes(xintercept = 2010), lty = 2) + # 2010
  geom_vline(aes(xintercept = 2014), lty = 2) + # 2014
  geom_vline(aes(xintercept = 2019), lty = 2) + 
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", hjust = 1, angle = 45),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))

FigS12D <- ggplot(all[data == "trap"]) +
  geom_point(aes(year, N, color = grid), size = 2) +
  ylab("Number of trapping events") +
  xlab("Year") +
  ggtitle('D)') +
  scale_color_manual(values = col) +
  geom_vline(aes(xintercept = 1998), lty = 2) + # 1998
  geom_vline(aes(xintercept = 2005), lty = 2) + # 2005
  geom_vline(aes(xintercept = 2010), lty = 2) + # 2010
  geom_vline(aes(xintercept = 2014), lty = 2) + # 2014
  geom_vline(aes(xintercept = 2019), lty = 2) + 
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", hjust = 1, angle = 45),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))

grid.arrange(FigS12A, FigS12B, 
             FigS12C, FigS12D, 
             nrow = 2)
dev.off()


a1 <- lm(scale(N) ~ spr_density + grid, data = all[data == "behaviour"])
summary(a1)
b1 <- lm(scale(N) ~ spr_density + grid, data = all[data == "trap"])
summary(b1)
