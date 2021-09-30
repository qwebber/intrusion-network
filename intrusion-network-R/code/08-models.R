

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
      year == 2019][, median(area_ha)]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(area_ha)]

## average territory size non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(area_ha)]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(area_ha)]


## average out-strength for mast years
all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, median(outstrength)]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(outstrength)]

## average intrusion strength non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(outstrength)]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(outstrength)]


## average in-strength for mast years
all[year == 1998 |
       year == 2005 | 
       year == 2010 | 
       year == 2014 | 
       year == 2019][, median(instrength)]

all[year == 1998 |
       year == 2005 | 
       year == 2010 | 
       year == 2014 | 
       year == 2019][, sd(instrength)]

## average intrusion strength non-mast years
all[year != 1998 |
       year != 2005 | 
       year != 2010 | 
       year != 2014 | 
       year != 2019][, median(instrength)]

all[year != 1998 |
       year != 2005 | 
       year != 2010 | 
       year != 2014 | 
       year != 2019][, sd(instrength)]



############################ 
#### INTRUSION STRENGTH ####
############################ 
p.var_str <- var(all$outstrength, na.rm = TRUE)

prior_out <- list(G=list(G1=list(V=diag(2)*(p.var_str/2), nu=4,
                                 alpha.V=diag(2)*p.var_str/2)),
                  R=list(V=diag(1)*(p.var_str/2), nu=4))

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
                    #nitt=420000,
                    #burnin=20000,
                    #thin=100,
                    verbose = TRUE,
                    data = all,
                    pr=TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)

saveRDS(mcmc1, "output/models/mcmc_strength.RDS")

#####################
## TERRITORY SIZE ##
#####################
p.var_area <- var(all$area_ha, na.rm = TRUE)

prior_area <- list(G=list(G1=list(V=diag(2)*(p.var_area/2), nu=4,
                                 alpha.V=diag(2)*p.var_area/2)),
                  R=list(V=diag(1)*(p.var_area/2), nu=4))

mcmc2 <- MCMCglmm(area_ha ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                    mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_area,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc2, "output/models/mcmc_territory.RDS")


#####################
## IN-STRENGTH ##
#####################
p.var_in <- var(all$instrength, na.rm = TRUE)

prior_in <- list(G=list(G1=list(V=diag(2)*(p.var_in/2), nu=4,
                                  alpha.V=diag(2)*p.var_in/2)),
                   R=list(V=diag(1)*(p.var_in/2), nu=4))

mcmc3 <- MCMCglmm(instrength ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_in,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc3, "output/models/mcmc_instrength.RDS")


############################
## OUT-STRENGTH BEHAV OBS ##
#############################
p.var_outB <- var(all$outstrength_behav, na.rm = TRUE)

prior_outB <- list(G=list(G1=list(V=diag(2)*(p.var_outB/2), nu=4,
                                alpha.V=diag(2)*p.var_outB/2)),
                 R=list(V=diag(1)*(p.var_outB/2), nu=4))

mcmc4 <- MCMCglmm(outstrength_behav ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_outB,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc4, "output/models/mcmc_outstrength_behav.RDS")

############################
## IN-STRENGTH BEHAV OBS ##
#############################
p.var_inB <- var(all$instrength_behav, na.rm = TRUE)

prior_inB <- list(G=list(G1=list(V=diag(2)*(p.var_inB/2), nu=4,
                                  alpha.V=diag(2)*p.var_inB/2)),
                   R=list(V=diag(1)*(p.var_inB/2), nu=4))

mcmc5 <- MCMCglmm(instrength_behav ~ 
                     grid +
                     sex + 
                     age + 
                     spr_density + 
                     mast,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_inB,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc5, "output/models/mcmc_instrength_behav.RDS")


## Tri-variate model
p.var_str <- var(all$outstrength, na.rm = TRUE)
prior2 <- list(R = list(V=diag(3)*(p.var_str/2), nu=4), ## 2x2 matrix for random effects 
               G = list(G1 = list(V = diag(6)*(p.var_str/2), nu=4, ## 4x4 matrix for among-individual section
                                      alpha.V = diag(6)*p.var_str/2)))

mcmc4 <- MCMCglmm(cbind(instrengthScale, 
                        outstrengthScale,
                        areaScale) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:poly(age, degree = 2) + 
                    trait:spr_density + 
                    trait:mast, 
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian", "gaussian"),
                  prior = prior2,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc4)
saveRDS(mcmc4, "output/models/mcmc_all.RDS")

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
