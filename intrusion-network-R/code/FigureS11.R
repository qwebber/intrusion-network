
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
mcmc_strength_behav <- readRDS("output/models/mcmc_outstrength_behav.RDS")
mcmc_in_behav <- readRDS("output/models/mcmc_instrength_behav.RDS")
mcmc_strength_trap <- readRDS("output/models/mcmc_outstrength_trap.RDS")
mcmc_in_trap <- readRDS("output/models/mcmc_instrength_trap.RDS")



## convert to BRN format

## Out-strength behav data
df_strength_behav <- cbind(all,
                     fit = predict(mcmc_strength_behav, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_strength_behav = setDT(df_strength_behav)[Type == "fit"]

## In-strength behav data
df_in_behav <- cbind(all, 
               fit = predict(mcmc_in_behav, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   instrengthScale = mean(instrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:instrengthScale)

df_in_behav = setDT(df_in_behav)[Type == "fit"]

## convert to BRN format

## outstrength trapping data
df_strength_trap <- cbind(all,
                     fit = predict(mcmc_strength_trap, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrengthScale = mean(outstrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:outstrengthScale)

df_strength_trap = setDT(df_strength_trap)[Type == "fit"]

## In-strength trapping data
df_in_trap <- cbind(all, 
               fit = predict(mcmc_in_trap, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   instrengthScale = mean(instrengthScale)) %>%
  tidyr::gather(Type, Value,
                fit:instrengthScale)

df_in_trap = setDT(df_in_trap)[Type == "fit"]

col <- c("#f1a340", "#998ec3")

png("figures/FigS11.png", width = 6000, height = 6000, units = "px", res = 600)
FigS11A <- ggplot(data = df_strength_behav) +
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

FigS11B <- ggplot(data = df_strength_trap) +
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
  ylab("Out-intrusion-strength (Trapping events)") +
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

FigS11C <- ggplot(data = df_in_behav) +
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

FigS11D <- ggplot(data = df_in_trap) +
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
  ylab("In-intrusion-strength (Trapping events)") +
  ggtitle('D)') +
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

grid.arrange(FigS11A, FigS11B, 
             FigS11C, FigS11D, nrow = 2)
dev.off()
