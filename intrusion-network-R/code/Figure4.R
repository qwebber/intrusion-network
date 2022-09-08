
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
mcmc_strength_behav <- readRDS("output/models/mcmc_outstrength_behav.RDS")
mcmc_instrength_behav <- readRDS("output/models/mcmc_instrength_behav.RDS")


## convert to BRN format
df_strength <- cbind(all,
                     fit = predict(mcmc_strength, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density, sex) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_strength = subset(df_strength, Type == "fit")

## Territory size
df_territory <- cbind(all,
                      fit = predict(mcmc_territory, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density, sex) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   areaScale = mean(areaScale)) %>%
  tidyr::gather(Type, Value,
                fit:areaScale)

df_territory = subset(df_territory, Type == "fit")

## In-strength
df_in <- cbind(all,
               fit = predict(mcmc_in, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density, sex) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   instrengthScale = mean(instrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:instrengthScale)

df_in = subset(df_in, Type == "fit")

col <- c("#f1a340", "#998ec3")

png("figures/Fig4.png", width = 8000, height = 4000, units = "px", res = 600)
Fig4A <- ggplot(data = df_strength) +
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

Fig4B <- ggplot(data = df_territory) +
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
  theme(legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5)) 

Fig4C <- ggplot(data = df_in) +
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

fig4D <- ggplot() +
  geom_smooth(data = df_strength, 
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

fig4E <- ggplot() +
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

fig4F <- ggplot() +
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
             Fig4A, Fig4B, Fig4C,
             fig4D, fig4E, fig4F, 
             ncol = 3, nrow = 2)
dev.off()



## convert to BRN format
df_strength_behav <- cbind(all,
                     fit = predict(mcmc_strength_behav, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, densityScale, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_strength_behav = subset(df_strength_behav, Type == "fit")
df_strength_behav <- df_strength_behav[!is.na(df_strength_behav$grid)]



ggplot(data = df_strength_behav) +
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

png("figures/extra.png", width = 3000, height = 3000, units = "px", res = 600)
ggplot(data = setDT(df_strength)[grid == "KL"]) +
  geom_smooth(aes(spr_density, Value, group = as.factor(squirrel_id)),
              color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  geom_smooth(aes(spr_density, Value), 
              method = lm, 
              color = "black",
              se = F) +
  scale_color_manual(values = "grey") +
  xlab("Spring density (squirrels/ha)") +
  ylab("Intrusion rates") +
  #ggtitle('A)') +
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
dev.off()
