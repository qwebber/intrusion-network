


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

all$sex[all$sex == "F"] <- "Female"
all$sex[all$sex == "M"] <- "Male"


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

png("figures/FigS9.png", width = 4000, height = 8000, units = "px", res = 600)
FigS9A <- ggplot(data = df_strength) +
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
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5)) +
  facet_wrap(~sex)

FigS9B <- ggplot(data = df_territory) +
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
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 0.5)) +
  facet_wrap(~sex)

FigS9C <- ggplot(data = df_in) +
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
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5)) +
  facet_wrap(~sex)

grid.arrange(FigS9A, FigS9B, FigS9C, nrow = 3)
dev.off()
