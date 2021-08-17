
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

## scale variables:
all[, outstrengthScale := scale(outstrength)]
all[, instrengthScale := scale(instrength)]
all[, areaScale := scale(area_ha)]
all[, densityScale := scale(spr_density)]

## load models
mcmc_strength <- readRDS("output/models/mcmc_strength.RDS")
mcmc_territory <- readRDS("output/models/mcmc_territory.RDS")
mcmc_in <- readRDS("output/models/mcmc_instrength.RDS")

## convert to BRN format
df_strength <- cbind(all,
                     fit = predict(mcmc_strength, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_fit_strength = setDT(df_strength)[Type == "fit"]
df_fit_strength <- df_fit_strength[!is.na(df_fit_strength$grid)]

## Territory size
df_territory <- cbind(all,
                      fit = predict(mcmc_territory, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   areaScale = mean(areaScale)) %>%
  tidyr::gather(Type, Value,
                fit:areaScale)

df_territory = setDT(df_territory)[Type == "fit"]
df_territory <- df_territory[!is.na(df_territory$grid)]

## In-strength
df_in <- cbind(all,
               fit = predict(mcmc_in, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   instrengthScale = mean(instrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:instrengthScale)

df_in = setDT(df_in)[Type == "fit"]
df_in <- df_in[!is.na(df_in$grid)]

col <- c("#f1a340", "#998ec3")

png("figures/Fig3.png", width = 8000, height = 4000, units = "px", res = 600)
Fig3A <- ggplot(data = df_fit_strength) +
  geom_smooth(aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  geom_smooth(aes(spr_density, Value), 
              method = lm, 
              color = "black") +
  scale_color_manual(values = col) +
  xlab("Spring density (squirrels/ha)") +
  ylab("Out-intrusion-strength") +
  ggtitle('A)') +
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

Fig3B <- ggplot(data = df_territory ) +
  geom_smooth(aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  geom_smooth(aes(spr_density, Value), 
              method = lm, 
              color = "black") +
  scale_color_manual(values = col) +
  #ylim(-0.8, 1) +
  xlab("Spring density (squirrels/ha)") +
  ylab("Territory size") +
  ggtitle('B)') +
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

Fig3C <- ggplot(data = df_in) +
  geom_smooth(aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  geom_smooth(aes(spr_density, Value), 
              method = lm, 
              color = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("Spring density (squirrels/ha)") +
  ylab("In-intrusion-strength") +
  ggtitle('C)') +
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

fig3D <- ggplot() +
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
  ylab("Out-intrusion-strength") +
  scale_color_manual(values = col) +
  ggtitle('D)') +
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

fig3E <- ggplot() +
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
  ylab("Territory size") +
  scale_color_manual(values = col) +
  ggtitle('E)') +
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

fig3F <- ggplot() +
  geom_smooth(data = df_in, 
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
  ylab("In-intrusion-strength") +
  scale_color_manual(values = col) +
  ggtitle('F)') +
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
grid.arrange(
             Fig3A, Fig3B, Fig3C,
             fig3D, fig3E, fig3F, 
             ncol = 3, nrow = 2)
dev.off()


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

