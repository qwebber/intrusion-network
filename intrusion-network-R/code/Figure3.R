
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
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]


## load models
mcmc_strength <- readRDS("output/models/mcmc_strength.RDS")
mcmc_territory <- readRDS("output/models/mcmc_territory.RDS")

## convert to BRN format
df_strength <- cbind(all,
                     fit = predict(mcmc_strength, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrength = mean(outstrength)) %>%
  tidyr::gather(Type, Value,
                fit:outstrength)

df_fit_strength = setDT(df_strength)[Type == "fit"]
df_fit_strength <- df_fit_strength[!is.na(df_fit_strength$grid)]

## Territory size
df_territory <- cbind(all,
                      fit = predict(mcmc_territory, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrength = mean(outstrength)) %>%
  tidyr::gather(Type, Value,
                fit:outstrength)

df_territory = setDT(df_territory)[Type == "fit"]
df_territory <- df_territory[!is.na(df_territory$grid)]


col <- c("#f1a340", "#998ec3")

## add 1998 as a dummy variable
df_fit_strength98 <- data.table(squirrel_id = 0,
                                grid = "KL",
                                year = 1998,
                                spr_density = 0,
                                Type = "NA",
                                Value = 0)

png("figures/Fig3.png", width = 3000, height = 6000, units = "px", res = 600)
fig3A <- ggplot(data = density[year >1995],
       aes(as.factor(year), spr_density, group = grid, color = grid)) +
  geom_point(size = 2) +
  scale_color_manual(values = col) +
  geom_line() +
  ylab("Spring density (squirrels/ha)") +
  xlab("Year") +
  ggtitle("A)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     limits = c(0, 4.5)) +
  scale_x_discrete(breaks = c(#1988, 1990, 1992, 1994,
                                1996, 1998, 
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
    legend.position = c(0.8, 0.9),
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

fig3B <- ggplot() +
  geom_smooth(data = df_fit_strength, 
              aes(year, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              method = lm,
              se = FALSE) +
  geom_vline(aes(xintercept = 3), lty = 2) + # 1998
  geom_vline(aes(xintercept = 10), lty = 2) + # 2005
  geom_vline(aes(xintercept = 15), lty = 2) + # 2010
  geom_vline(aes(xintercept = 19), lty = 2) + # 2014
  geom_vline(aes(xintercept = 24), lty = 2) + # 2019
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
  xlab("Year") +
  ylim(-0.8, 1) +
  ylab("Intrusion strength") +
  scale_color_manual(values = col) +
  ggtitle('B)') +
  theme(
    legend.position = 'none',
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

fig3C <- ggplot() +
  geom_smooth(data = df_territory, 
              aes(year, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              method = lm,
              se = FALSE) +
  geom_vline(aes(xintercept = 3), lty = 2) + # 1998
  geom_vline(aes(xintercept = 10), lty = 2) + # 2005
  geom_vline(aes(xintercept = 15), lty = 2) + # 2010
  geom_vline(aes(xintercept = 19), lty = 2) + # 2014
  geom_vline(aes(xintercept = 24), lty = 2) + # 2019
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
                                "2020"))  +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + 
  xlab("Year") +
  ylab("Territory size (ha)") +
  scale_color_manual(values = col) +
  ggtitle('C)') +
  theme(
    legend.position = 'none',
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
grid.arrange(fig3A,
             fig3B,
             fig3C, ncol = 1, nrow = 3)
dev.off()
