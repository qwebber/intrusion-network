

### Packages ----
libs <- c('data.table',
          'ggplot2', 'gridExtra',
          'lme4',
          'visreg',
          'glmmTMB', 
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

density <- readRDS("output/auxilliary-data/spring-density.RDS")
density <- setDT(density)[year > 1995] 
density <- density[grid == "KL" | grid == "SU"]

### Reaction Norm for KDE ----
p.var_str <- var(all$outstrength, na.rm = TRUE)

prior <- list(G=list(G1=list(V=diag(2)*(p.var_str/2), nu=1,
                                 alpha.V=diag(2)*p.var_str/2)),
                  R=list(V=diag(1)*(p.var_str/2), nu=1))

mcmc1 <- MCMCglmm(scale(outstrength) ~ 
                      grid +
                      sex + 
                      age + 
                      spr_density,
                    random =~ us(1 + spr_density):squirrel_id,
                    rcov = ~units,
                    family = "gaussian",
                    prior = prior,
                    #nitt=420000,
                    #burnin=20000,
                    #thin=100,
                    verbose = TRUE,
                    data = all,
                    pr=TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)

#saveRDS(mcmcKDE, "data/derived-data/MCMC-models/mod_KDE.RDS")


## KDE
df_strength <- cbind(all,
                fit = predict(mcmc1, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
            outstrength = mean(outstrength)) %>%
  tidyr::gather(Type, Value,
         fit:outstrength)

df_fit_strength = setDT(df_strength)[Type == "fit"]

df_fit_strength <- df_fit_strength[!is.na(df_fit_strength$grid)]

png("figures/Fig4.png", height = 6000, width = 3000, units = "px", res = 600)
aa <- ggplot() +
  geom_smooth(data = df_fit_strength, 
    aes(spr_density, Value, group = as.factor(squirrel_id), color = grid),
    #color = "darkgrey",
    size = 0.25,
    alpha = 0.5,
    method = lm,
    se = FALSE) +
  scale_color_manual(values = c("orange", "royalblue")) +
  #geom_line(data = density, aes(as.factor(year), spr_density, group = grid, color = grid)) +
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

bb <- ggplot() +
  geom_smooth(data = df_fit_strength, 
              aes(as.integer(year), Value, group = as.factor(squirrel_id), color = grid),
              #color = "darkgrey",
              size = 0.25,
              method = lm,
              se = FALSE) +
  geom_vline(aes(xintercept = 3), lty = 2) + # 1998
  geom_vline(aes(xintercept = 10), lty = 2) + # 2005
  geom_vline(aes(xintercept = 15), lty = 2) + # 2010
  geom_vline(aes(xintercept = 19), lty = 2) + # 2014
  geom_vline(aes(xintercept = 24), lty = 2) + # 2019
  scale_x_continuous(breaks = c(1, 3, 5, 
                                7, 9, 11, 
                                13, 15, 17, 
                                19, 21, 23, 25),
                     labels = c("1996", "1998", "2000", 
                                "2002", "2004", "2006",
                                "2008", "2010", "2012", 
                                "2014", "2016", "2018", "2020")) +
  xlab("Year") +
  ylim(-0.8, 1) +
  ylab("Intrusion strength") +
  scale_color_manual(values = c("orange", "royalblue")) +
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

grid.arrange(aa,bb, nrow = 2)
dev.off()


summary(mcmc1)


mcmc2 <- MCMCglmm(scale(area_ha) ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density +
                    I(spr_density)^2,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

#saveRDS(mcmcKDE, "data/derived-data/MCMC-models/mod_KDE.RDS")


## KDE
df_area <- cbind(all,
                 fit = predict(mcmc2, marginal = NULL)) %>%
  group_by(squirrel_id, grid, year, spr_density) %>%
  dplyr::summarise(fit = mean(fit.V1),
                   outstrength = mean(area_ha)) %>%
  tidyr::gather(Type, Value,
                fit:outstrength)

df_fit_area = setDT(df_area)[Type == "fit"]


ggplot(df_fit_area, 
             aes(x = year, y = Value, group = factor(squirrel_id))) +
  geom_smooth(
    aes(year, Value, group = squirrel_id, color = grid),
    #color = "darkgrey",
    size = 0.25,
    method = lm,
    se = FALSE
  ) +
  xlab("Spring density (squirrels/ha)") +
  ylab("Territory size (ha)") +
  #ggtitle('A') +
  theme(
    #legend.position = 'none',
    plot.title = element_text(size = 9),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5))





ggplot(all) +
  geom_point(aes(spr_density, area_ha, color = sex, shape = grid)) 
  facet_wrap(~year)

aa <- ggplot(filter(vis_mod1$fit), aes(spr_density, visregFit))+
          geom_line(colour = 'black', 
                    size=1)+
          geom_jitter(data = filter(vis_mod1$res),
                             aes(spr_density, visregRes), 
                             alpha = 0.8, 
                             color = "#d8b365") +
          xlab('Density') +
          ylab('Out-strength') + 
          theme(legend.position = c(0.75,0.8),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 10),
                axis.title = element_text(size = 14, color = 'black'),
                axis.text = element_text(size = 12, color = 'black'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                panel.border = element_rect(colour = "black", fill=NA, size = 1))
bb <- ggplot(filter(vis_mod2$fit), aes(spr_density, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_jitter(data = filter(vis_mod2$res),
              aes(spr_density, visregRes), 
              alpha = 0.8, 
              color = "#5ab4ac") +
  xlab('Density') +
  ylab('In-strength') + 
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
grid.arrange(aa,bb)





## model 1: outstrength
mod1 <- glmmTMB(log(area_ha + 1) ~ #age + I(age^2) + 
                  #grid + 
                  age +
                  sex + 
                  #I(age)^2 +
                  spr_density + 
                  #I(spr_density^2) +
                  (1|year) + 
                  (1|grid/squirrel_id), 
                data=all)
summary(mod1)

## model 2: instrength
mod2 <- glmmTMB(log(area_ha + 1) ~ #age + I(age^2) + 
                  grid + 
                  age +
                  sex +
                  spr_density + 
                  #I(spr_density^2) +
                  (1|year) + 
                  (1|squirrel_id), 
                data=all)
summary(mod2)

vis_mod1 <- visreg(mod1, "spr_density", xlab="Density", ylab="Out-strength") 
vis_mod2 <- visreg(mod2, "spr_density", xlab="Density", ylab="In-strength") 
