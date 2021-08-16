

### Packages ----
libs <- c('data.table','dplyr', 'MCMCglmm')
lapply(libs, require, character.only = TRUE)

## load data
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

all <- all[gr_year != "KL_2006"]

all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

## scale variables within year:
all[, outstrengthScale := scale(outstrength)]
all[, instrengthScale := scale(instrength)]
all[, areaScale := scale(area_ha)]
all[, densityScale := scale(spr_density)]


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
                      mast + 
                      N,
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
                    mast + 
                     N,
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
                     mast + 
                     N,
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


## Bivariate model
p.var_str <- var(all$outstrength, na.rm = TRUE)
prior2 <- list(R = list(V=diag(2)*(p.var_str/2), nu=4), ## 2x2 matrix for random effects 
               G = list(G1 = list(V = diag(4)*(p.var_str/2), nu=4, ## 4x4 matrix for among-individual section
                                      alpha.V = diag(4)*p.var_str/2)))

mcmc4 <- MCMCglmm(cbind(instrengthScale, 
                        outstrengthScale) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:age + 
                    trait:spr_density + 
                    trait:mast,
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian"),
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
saveRDS(mcmc4, "output/models/mcmc_instrength-outstrength.RDS")


mcmc5 <- MCMCglmm(cbind(areaScale, 
                        outstrengthScale) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:age + 
                    trait:spr_density + 
                    trait:mast,
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian"),
                  prior = prior2,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc5)
saveRDS(mcmc5, "output/models/mcmc_area-outstrength.RDS")



mcmc6 <- MCMCglmm(cbind(areaScale, 
                        instrengthScale) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:age + 
                    trait:spr_density + 
                    trait:mast,
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait):units,
                  family = c("gaussian","gaussian"),
                  prior = prior2,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc6)
saveRDS(mcmc6, "output/models/mcmc_area-intstrength.RDS")


