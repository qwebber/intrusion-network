


### Packages ----
libs <- c('data.table','dplyr', 'MCMCglmm', 
          'ggplot2' ,'coda', 'tidybayes',
          'emmeans', 'bayesplot')
lapply(libs, require, character.only = TRUE)


## load data
## load data
all <- readRDS("output/final-df.RDS")
all <- all[gr_year != "KL_2006"]
all$squirrel_id <- as.factor(all$squirrel_id)
## scale variables:
all[, outstrengthScale := scale(outstrength)]
all[, instrengthScale := scale(instrength)]
all[, areaScale := scale(area_ha)]
all[, densityScale := scale(spr_density)]
all[, NScale := scale(N)]
all[, yrScale := scale(as.numeric(year))]

unique(all$gr_year)
all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

## load models
mcmc4 <- readRDS("output/models/mcmc_area-outstrength.RDS")
summary(mcmc4)
mcmc5 <- readRDS("output/models/mcmc_area-instrength.RDS")
summary(mcmc5)
mcmc6 <- readRDS("output/models/mcmc_instrength-outstrengthScale.RDS")
summary(mcmc6)

y <- as.vector(all$outstrengthScale)   # the DV that was used for the MCMCglmm model

# generating the simulated data using the simulate.MCMCglmm function from the MCMCglmm package
yrep <- simulate.MCMCglmm(mcmc4, 1861) 

#yrep <- t( yrep ) # transposing yrep for the bayesplot functions


# ppc_stat: A histogram of the distribution of a test statistic computed by applying stat to each 
#           dataset (row) in yrep. The value of the statistic in the observed data, stat(y), is 
#           overlaid as a vertical line.
ppc_dens_overlay(y, yrep)
ppc_stat(y, yrep[1:1861])


hist(log(y))
hist(yrep)
## calculate e means
df <- mcmc4 %>%
  emmeans(~ grid + sex + mast, data = all) %>%
  gather_emmeans_draws() %>%
  median_qi()


## Plot the posterior distributions of the 2 response variables from the bivariate model
ggplot(df, aes(.value, sex, color = grid, shape = mast)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = .lower, 
                     xmax = .upper, 
                     color = grid, 
                     height = 0),
                 position = position_dodge(width = 0.4)) +
  ylab("") +
  xlab("Posterior distribution") +
  theme_bw() +
  facet_wrap(~trait)

# Visualize posterior distributions for outstrength - territory size model

plot(mcmc4$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitareaScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitareaScale:spr_density.squirrel_id"])


## Heidel test
mcmc4_heidel <- heidel.diag(mcmc4$Sol, eps = 0.1, pvalue = 0.05)
mcmc4_heidel2 <- data.table(mcmc4_heidel[1:4130,])

hist(mcmc4_heidel2$halfwidth)

## autocorrelation -- we basically want these numbesr to be <0.1
autocorr.diag(mcmc4$VCV)



# Visualize posterior distributions for instrength - territory size model

plot(mcmc5$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:traitinstrengthScale.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:traitareaScale.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:traitareaScale.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc5$VCV[,"traitinstrengthScale:traitareaScale:spr_density.squirrel_id"])
plot(mcmc5$VCV[,"traitareaScale:spr_density:traitareaScale:spr_density.squirrel_id"])


## Heidel test
mcmc5_heidel <- heidel.diag(mcmc5$Sol, eps = 0.1, pvalue = 0.05)
mcmc5_heidel2 <- data.table(mcmc5_heidel[1:4130,])

hist(mcmc5_heidel2$halfwidth)

## autocorrelation -- we basically want these numbesr to be <0.1
autocorr.diag(mcmc5$VCV)



# Visualize posterior distributions for instrength - outstrength model

plot(mcmc6$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:traitinstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc6$VCV[,"traitinstrengthScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc6$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])


## Heidel test
mcmc6_heidel <- heidel.diag(mcmc6$Sol, eps = 0.1, pvalue = 0.05)
mcmc6_heidel2 <- data.table(mcmc6_heidel[1:4130,])

hist(mcmc6_heidel2$halfwidth)

## Gelman-Ruben
gelman.diag(mcmc6$Sol)

## autocorrelation -- we basically want these numbesr to be <0.1
autocorr.diag(mcmc5$VCV)