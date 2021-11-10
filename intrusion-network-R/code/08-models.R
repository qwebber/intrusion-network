

### Packages ----
libs <- c('data.table','dplyr', 'MCMCglmm')
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

## scale variables:
all[, outstrengthScale := scale(outstrength)]
all[, instrengthScale := scale(instrength)]
all[, areaScale := scale(area_ha)]
all[, densityScale := scale(spr_density)]
all[, NScale := scale(N)]
all[, yrScale := scale(as.numeric(year))]

########################
#### SUMMARY STATS ####
#######################  

## number of unique individuals
length(unique(all$squirrel_id))

## number of squirrel years
length(unique(all$id_yr_gr))

## number of years per ID
mean(all[, .N, by = "squirrel_id"]$N)
sd(all[, .N, by = "squirrel_id"]$N)
range(all[, .N, by = "squirrel_id"]$N)

## average territory size mast years
all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, median(area_ha), by = "sex"]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(area_ha), by = "sex"]

## average territory size non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(area_ha), by = "sex"]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(area_ha), by = "sex"]


## average out-strength for mast years
all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, median(outdegree), by = "sex"]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(outdegree), by = "sex"]

## average out-intrusion strength non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(outdegree), by = "sex"]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(outdegree), by = "sex"]


## average in-strength for mast years
all[year == 1998 |
       year == 2005 | 
       year == 2010 | 
       year == 2014 | 
       year == 2019][, median(indegree), by = "sex"]

all[year == 1998 |
       year == 2005 | 
       year == 2010 | 
       year == 2014 | 
       year == 2019][, sd(indegree), by = "sex"]

## average in-intrusion strength non-mast years
all[year != 1998 |
       year != 2005 | 
       year != 2010 | 
       year != 2014 | 
       year != 2019][, median(indegree), by = "sex"]

all[year != 1998 |
       year != 2005 | 
       year != 2010 | 
       year != 2014 | 
       year != 2019][, sd(indegree), by = "sex"]



############################ 
#### INTRUSION STRENGTH ####
############################ 
prior_out <- list(G=list(G1=list(V=diag(2)*1, nu=1,
                                 alpha.V=diag(2)*25^2)),
                  R=list(V=diag(1)*1, nu=1))

mcmc1 <- MCMCglmm(outstrength ~ 
                      grid +
                      sex + 
                      age + 
                      spr_density + 
                      mast,
                    random =~ us(1 + spr_density):squirrel_id,
                    rcov = ~units,
                    family = "gaussian",
                    prior = prior_out,
                    nitt=420000,
                    burnin=20000,
                    thin=100,
                    verbose = TRUE,
                    data = all,
                    pr=TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)

saveRDS(mcmc1, "output/models/mcmc_strength.RDS")

#####################
## TERRITORY SIZE ##
#####################
mcmc2 <- MCMCglmm(area_ha ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                    mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc2, "output/models/mcmc_territory.RDS")


#####################
## IN-STRENGTH ##
#####################
mcmc3 <- MCMCglmm(instrength ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc3, "output/models/mcmc_instrength.RDS")


############################
## OUT-STRENGTH BEHAV OBS ##
#############################
mcmc4 <- MCMCglmm(outstrength_behav ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc4, "output/models/mcmc_outstrength_behav.RDS")

############################
## IN-STRENGTH BEHAV OBS ##
#############################
mcmc5 <- MCMCglmm(instrength_behav ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc5, "output/models/mcmc_instrength_behav.RDS")

############################
## OUT-STRENGTH TRAPPING ##
#############################
mcmc6 <- MCMCglmm(outstrength_trap ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc6, "output/models/mcmc_outstrength_trap.RDS")

############################
## IN-STRENGTH BEHAV OBS ##
#############################
mcmc7 <- MCMCglmm(instrength_trap ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_out,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc7, "output/models/mcmc_instrength_trap.RDS")

## Bi-variate model - area - outstrength
prior1 <- list(R = list(V=diag(2)*1, nu=1), ## 3x3 matrix for random effects 
               G = list(G1 = list(V = diag(4)*1, nu=1, ## 6x6 matrix for among-individual section
                                  alpha.V = diag(4)*25^2)))

mcmc8 <- MCMCglmm(cbind(outstrengthScale,
                         areaScale) ~ 
                      trait-1 + 
                      trait:grid +
                      trait:sex + 
                      trait:age + 
                      trait:spr_density + 
                      trait:mast +
                      trait:yrScale, 
                   random =~ us(trait + spr_density:trait):squirrel_id,
                   rcov =~ idh(trait):units,
                   family = c("gaussian","gaussian"),
                   prior = prior1,
                   nitt=420000,
                   burnin=20000,
                   thin=100,
                   verbose = TRUE,
                   data = all,
                   pr=TRUE,
                   saveX = TRUE,
                   saveZ = TRUE)

summary(mcmc8)
saveRDS(mcmc8, "output/models/mcmc_area-outstrength.RDS")

## Bi-variate model - area - instrength
mcmc9 <- MCMCglmm(cbind(instrengthScale,
                        areaScale) ~ 
                     trait-1 + 
                     trait:grid +
                     trait:sex + 
                     trait:age + 
                     trait:spr_density + 
                     trait:mast +
                     trait:yrScale, 
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian"),
                  prior = prior1,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc9)
saveRDS(mcmc9, "output/models/mcmc_area-instrength.RDS")


## Bi-variate model - area - instrength
mcmc10 <- MCMCglmm(cbind(instrengthScale,
                         outstrengthScale) ~ 
                     trait-1 + 
                     trait:grid +
                     trait:sex + 
                     trait:age + 
                     trait:spr_density + 
                     trait:mast +
                     trait:yrScale, 
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian"),
                  prior = prior1,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc10)
saveRDS(mcmc10, "output/models/mcmc_instrength-outstrengthScale.RDS")



## Tri-variate model
prior2 <- list(R = list(V=diag(3)*1, nu=1), ## 3x3 matrix for random effects 
               G = list(G1 = list(V = diag(6)*1, nu=1, ## 6x6 matrix for among-individual section
                                      alpha.V = diag(6)*25^2)))
mcmc11 <- MCMCglmm(cbind(instrengthScale, 
                        outstrengthScale,
                        areaScale) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:age + 
                    trait:spr_density + 
                    trait:mast +
                    trait:yrScale, 
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian", "gaussian"),
                  prior = prior2,
                  nitt=420000,
                  burnin=20000,
                  thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc11)
saveRDS(mcmc11, "output/models/mcmc_all.RDS")

library(broom.mixed)

coefs <- setDT(broom.mixed::tidy(mcmc4))
coefs <- coefs[effect == "fixed"]

coefs2 <- coefs[, c("trait", "variable") := tstrsplit(term, ":", fixed=TRUE)][,c("term") := NULL]

## rename variables
coefs2$variable[1:3] <- "Intercept"
coefs2$variable[coefs2$variable == "gridSU"] <- "Grid (SU)"
coefs2$variable[coefs2$variable == "sexM"] <- "Sex (M)"
coefs2$variable[coefs2$variable == "age"] <- "Age"
coefs2$variable[coefs2$variable == "spr_density"] <- "Density"
coefs2$variable[coefs2$variable == "mastnomast"] <- "Mast (No mast)"
coefs2$variable[coefs2$variable == "N"] <- "Number of locations"
coefs2$trait[coefs2$trait == "traitareaScale"] <- "Territory size"
coefs2$trait[coefs2$trait == "traitoutstrengthScale"] <- "Out-intrusion-strength"
coefs2$trait[coefs2$trait == "traitinstrengthScale"] <- "In-intrusion-strength"

ggplot(coefs2, aes(variable, estimate)) + 
   geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error, color = trait),
              width = 0, 
              size = 0.55,
              position = position_dodge(width = 0.5)) +
   geom_point(aes(color = trait), 
              position = position_dodge(width = 0.5), 
              size = 2) +
   scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
   xlab("") +
   ylab("Fixed effect coefficient (+/- 95% CI)") +
   coord_flip() +
   geom_hline(yintercept = 0, linetype = 'dotted') +
   geom_vline(xintercept = 0.5, linetype = 'dotted') +
   geom_vline(xintercept = 1.5, linetype = 'dotted') +
   geom_vline(xintercept = 2.5, linetype = 'dotted') +
   geom_vline(xintercept = 3.5, linetype = 'dotted') +
   geom_vline(xintercept = 4.5, linetype = 'dotted') +
   geom_vline(xintercept = 5.5, linetype = 'dotted') +
   geom_vline(xintercept = 6.5, linetype = 'dotted') +
   geom_vline(xintercept = 7.5, linetype = 'dotted') +
   theme(#legend.position = 'none',
      legend.title = element_text(size = 14, color = "black"),
      legend.key = element_blank(),
      axis.title = element_text(size = 14, color = 'black'),
      axis.text = element_text(size = 12, color = 'black'),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      panel.border = element_rect(colour = "black", fill=NA, size = 1))


a1 <- lmer(outstrengthScale ~ grid +
              #sex + 
              poly(age, degree = 2) + 
              spr_density + 
              mast + 
              (1|squirrel_id), 
              data = all[sex == "M"])

visreg(a1)
