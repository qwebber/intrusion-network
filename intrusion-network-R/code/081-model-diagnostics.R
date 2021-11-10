


### Packages ----
libs <- c('data.table','dplyr', 'MCMCglmm', 
          'ggplot2' ,'coda', 'tidybayes',
          'emmeans')
lapply(libs, require, character.only = TRUE)

install.packages("tidybayes")

## load data
## load data
all <- readRDS("output/final-df.RDS")
all <- all[gr_year != "KL_2006"]
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)
all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

## load model
mcmc4 <- readRDS("output/models/mcmc_strength.RDS")

df <- mcmc4 %>%
  emmeans(~ grid + sex + mast, data = all) %>%
  gather_emmeans_draws() %>%
  median_qi()

ggplot(df, aes(.value, sex, color = grid)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = .lower, 
                     xmax = .upper, 
                     color = grid, 
                     height = 0),
                 position = position_dodge(width = 0.2)) +
  ylab("") +
  xlab("Posterior distribution") +
  theme_bw() +
  facet_wrap(~mast)

# Check posterior distributions
par(mfrow = c(1,1))
plot(mcmc4$Sol)

summary(mcmc4)

plot(mcmc4$VCV[, "(Intercept):(Intercept).squirrel_id"])
plot(mcmc4$VCV[, "spr_density:(Intercept).squirrel_id"])
plot(mcmc4$VCV[, "units"])


## visually examine the trace of variable across iterations
plot(mcmc4$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"], 
     main = "traitinstrengthScale:traitinstrengthScale.squirrel_id")
plot(mcmc4$VCV[,"traitoutstrengthScale:traitinstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitinstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitinstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitoutstrengthScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitareaScale.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitinstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitoutstrengthScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitinstrengthScale:traitareaScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitoutstrengthScale:traitareaScale:spr_density.squirrel_id"])
plot(mcmc4$VCV[,"traitareaScale:traitareaScale:spr_density.squirrel_id    "])
plot(mcmc4$VCV[,"traitinstrengthScale:spr_density:traitareaScale:spr_density.squirrel_id "])
plot(mcmc4$VCV[,"traitoutstrengthScale:spr_density:traitareaScale:spr_density.squirrel_id "])
plot(mcmc4$VCV[,"traitareaScale:spr_density:traitareaScale:spr_density.squirrel_id"])


m6 <- lapply(mcmc4, function(m) mcmc4$Sol)
m6 <- do.call(mcmc.list, m6)

heidel.diag(m6)

