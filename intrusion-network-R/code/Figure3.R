

### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load data
density <- readRDS("output/auxilliary-data/spring-density.RDS")
#density <- setDT(density)[year > 1995] 
density <- setDT(density)[grid == "KL" | grid == "SU"]

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

png("figures/Fig3.png", width = 2500, height = 2500, units = "px", res = 600)
ggplot(data = density[year >1995],
                aes(as.factor(year), spr_density, group = grid, color = grid)) +
  geom_point(size = 2) +
  scale_color_manual(values = col) +
  geom_line() +
  ylab("Spring density (squirrels/ha)") +
  xlab("Year") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     limits = c(0, 4.5)) +
  scale_x_discrete(breaks = c(1996, 1998, 
                              2000, 2002, 
                              2004, 2006, 
                              2008, 2010, 
                              2012, 2014,
                              2016, 2018, 
                              2020),
                   labels = c("1996", "1998", 
                              "2000", "2002", 
                              "2004", "2006",
                              "2008", "2010", 
                              "2012", "2014", 
                              "2016", "2018", 
                              "2020")) +
  geom_vline(aes(xintercept = "1993"), lty = 2) + # 1993
  geom_vline(aes(xintercept = "1998"), lty = 2) + # 1998
  geom_vline(aes(xintercept = "2005"), lty = 2) + # 2005
  geom_vline(aes(xintercept = "2010"), lty = 2) + # 2010
  geom_vline(aes(xintercept = "2014"), lty = 2) + # 2014
  geom_vline(aes(xintercept = "2019"), lty = 2) + # 2019
  theme(
    legend.position = c(0.85, 0.9),
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
