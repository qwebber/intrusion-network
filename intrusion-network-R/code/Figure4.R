
### Packages ----
libs <- c('data.table','dplyr',
          'ggplot2', 'gridExtra', 
          'MCMCglmm')
lapply(libs, require, character.only = TRUE)

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

png("figures/Fig4.png", height = 3000, width = 6000, units = "px", res = 600)
Fig4A <- ggplot() +
  geom_smooth(data = df_fit_strength, 
              aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  scale_color_manual(values = col) +
  ylim(-0.8, 1) +
  xlab("Spring density (squirrels/ha)") +
  ylab("Intrusion strength") +
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

Fig4B <- ggplot() +
  geom_smooth(data = df_territory , 
              aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              alpha = 0.5,
              method = lm,
              se = FALSE) +
  scale_color_manual(values = col) +
  #ylim(-0.8, 1) +
  xlab("Spring density (squirrels/ha)") +
  ylab("Territory size (ha)") +
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

grid.arrange(Fig4A, Fig4B, nrow = 2)
dev.off()
