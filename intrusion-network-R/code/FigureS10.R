
### Packages ----
libs <- c('data.table','dplyr',
          'ggplot2', 'gridExtra', 
          'MCMCglmm')
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

## scale variables within year:
all[, outstrengthScale := scale(outstrength), by = c("year", "grid")]
all[, instrengthScale := scale(instrength), by = c("year", "grid")]
all[, areaScale := scale(area_ha), by = c("year", "grid")]
all[, densityScale := scale(spr_density)]

## load models
mcmc_strength <- readRDS("output/models/mcmc_outstrength_behav.RDS")
mcmc_in <- readRDS("output/models/mcmc_instrength_behav.RDS")

## convert to BRN format
df_strength <- cbind(all,
                     fit = predict(mcmc_strength, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_fit_strength = setDT(df_strength)[Type == "fit"]

## In-strength
df_in <- cbind(all, 
               fit = predict(mcmc_in, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   instrengthScale = mean(instrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:instrengthScale)

df_in = setDT(df_in)[Type == "fit"]


col <- c("#f1a340", "#998ec3")

png("figures/FigS10.png", width = 8000, height = 4000, units = "px", res = 600)
FigS10A <- ggplot(data = df_fit_strength) +
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
  ylab("Out-intrusion-strength (Behavioural observations)") +
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

FigS10B <- ggplot(data = df_in) +
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
  ylab("In-intrusion-strength (Behavioural observations)") +
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

grid.arrange(FigS10A, FigS10B, nrow = 1)
dev.off()
